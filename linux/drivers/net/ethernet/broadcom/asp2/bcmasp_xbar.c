// SPDX-License-Identifier: GPL-2.0
/*
 * Broadcom STB ASP 2.0 Driver, OUTDMA and SPB_RX cross bar network device
 *
 * Copyright (c) 2020 Broadcom
 */
#define pr_fmt(fmt)		"bcmasp_xbar: " fmt

#include <linux/etherdevice.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/platform_device.h>
#include <linux/of.h>
#include <linux/of_address.h>
#include <linux/of_platform.h>
#include <linux/clk.h>
#include <net/rtnetlink.h>

#include "bcmasp.h"
#include "bcmasp_intf_defs.h"

static inline void bcmasp_xbar_enable_tx(struct bcmasp_intf *intf, int en)
{
	if (en)
		rx_spb_ctrl_wl(intf, RX_SPB_CTRL_ENABLE_EN,
			       RX_SPB_CTRL_ENABLE);
	else
		rx_spb_ctrl_wl(intf, 0x0, RX_SPB_CTRL_ENABLE);
}

static inline void bcmasp_xbar_enable_rx(struct bcmasp_intf *intf, int en)
{
	if (en)
		outdma_wl(intf, OUTDMA_ENABLE_EN, OUTDMA_ENABLE);
	else
		outdma_wl(intf, 0x0, OUTDMA_ENABLE);
}

static int bcmasp_xbar_init_rx(struct bcmasp_intf *intf)
{
	struct device *kdev = &intf->parent->pdev->dev;
	struct page *buffer_pg;
	dma_addr_t dma;
	void *p;
	int ret;

	intf->rx_buf_order = get_order(RING_BUFFER_SIZE);
	buffer_pg = alloc_pages(GFP_KERNEL, intf->rx_buf_order);

	dma = dma_map_page(kdev, buffer_pg, 0, RING_BUFFER_SIZE,
			   DMA_FROM_DEVICE);
	if (dma_mapping_error(kdev, dma)) {
		__free_pages(buffer_pg, intf->rx_buf_order);
		return -ENOMEM;
	}

	intf->rx_ring_cpu = page_to_virt(buffer_pg);
	intf->rx_ring_dma = dma;
	intf->rx_ring_dma_read = dma;
	intf->rx_ring_dma_valid = intf->rx_ring_dma + RING_BUFFER_SIZE - 1;

	p = dma_alloc_coherent(kdev, DESC_RING_SIZE, &intf->rx_edpkt_dma_addr,
			       GFP_KERNEL);
	if (!p) {
		ret = -ENOMEM;
		goto free_rx_ring;
	}

	intf->rx_edpkt_cpu = p;
	intf->rx_edpkt_dma_read = intf->rx_edpkt_dma_addr;

	intf->rx_edpkt_index = 0;

	netif_napi_add(intf->ndev, &intf->rx_napi, bcmasp_rx_poll,
		       NAPI_POLL_WEIGHT);

	/* Make sure channels are disabled */
	outdma_wl(intf, 0x0, OUTDMA_ENABLE);

	/* OUTDMA data buffers */
	outdma_wq(intf, intf->rx_ring_dma, OUTDMA_DATA_DMA_READ);
	outdma_wq(intf, intf->rx_ring_dma, OUTDMA_DATA_DMA_WRITE);
	outdma_wq(intf, intf->rx_ring_dma, OUTDMA_DATA_DMA_BASE);
	outdma_wq(intf, intf->rx_ring_dma_valid, OUTDMA_DATA_DMA_END);
	outdma_wq(intf, intf->rx_ring_dma_valid, OUTDMA_DATA_DMA_VALID);

	/* OUTDMA configuration: 64b alignment, 2b stuffing and push timer
	 * enable
	 */
	outdma_wl(intf, OUTDMA_DATA_CTRL1_E_64_ALN |
		  OUTDMA_DATA_CTRL1_E_STUFF |
		  OUTDMA_DATA_CTRL1_PSH_TMR_EN,
		  OUTDMA_DATA_CTRL1);
	outdma_wl(intf, 0xd2f0, OUTDMA_DATA_PUSH_TIMER);

	/* OUTDMA descriptors */
	outdma_wq(intf, intf->rx_edpkt_dma_addr, OUTDMA_DESC_DMA_WRITE);
	outdma_wq(intf, intf->rx_edpkt_dma_addr, OUTDMA_DESC_DMA_READ);
	outdma_wq(intf, intf->rx_edpkt_dma_addr, OUTDMA_DESC_DMA_BASE);
	outdma_wq(intf, intf->rx_edpkt_dma_addr + (DESC_RING_SIZE - 1),
		  OUTDMA_DESC_DMA_END);
	outdma_wq(intf, intf->rx_edpkt_dma_addr + (DESC_RING_SIZE - 1),
		  OUTDMA_DESC_DMA_VALID);
	outdma_wl(intf, 0xd2f0, OUTDMA_DESC_PUSH_TIMER);

	return 0;

free_rx_ring:
	dma_unmap_page(kdev, intf->rx_ring_dma, RING_BUFFER_SIZE,
		       DMA_FROM_DEVICE);
	__free_pages(virt_to_page(intf->rx_ring_cpu), intf->rx_buf_order);

	return ret;
}

static int bcmasp_xbar_init_tx(struct bcmasp_intf *intf)
{
	struct device *kdev = &intf->parent->pdev->dev;
	void *p;
	int ret;

	p = dma_alloc_coherent(kdev, DESC_RING_SIZE, &intf->tx_spb_dma_addr,
			       GFP_KERNEL);
	if (!p)
		return -ENOMEM;

	intf->tx_spb_cpu = p;
	intf->tx_spb_dma_valid = intf->tx_spb_dma_addr + DESC_RING_SIZE - 1;
	intf->tx_spb_dma_read = intf->tx_spb_dma_addr;

	intf->tx_cbs = kcalloc(DESC_RING_COUNT, sizeof(struct bcmasp_tx_cb),
			       GFP_KERNEL);
	if (!intf->tx_cbs) {
		ret = -ENOMEM;
		goto free_tx_spb;
	}

	spin_lock_init(&intf->tx_lock);
	intf->tx_spb_index = 0;
	intf->tx_spb_clean_index = 0;

	netif_tx_napi_add(intf->ndev, &intf->tx_napi, bcmasp_tx_poll,
			  NAPI_POLL_WEIGHT);

	/* Make sure channels are disabled */
	rx_spb_ctrl_wl(intf, 0x0, RX_SPB_CTRL_ENABLE);

	/* Rx SPB */
	rx_pause_ctrl_wl(intf, BIT(intf->channel), RX_PAUSE_MAP_VECTOR);
	rx_spb_top_wl(intf, 0x1e, RX_SPB_TOP_BLKOUT);

	rx_spb_dma_wq(intf, intf->tx_spb_dma_addr, RX_SPB_DMA_READ);
	rx_spb_dma_wq(intf, intf->tx_spb_dma_addr, RX_SPB_DMA_BASE);
	rx_spb_dma_wq(intf, intf->tx_spb_dma_valid, RX_SPB_DMA_END);
	rx_spb_dma_wq(intf, intf->tx_spb_dma_valid, RX_SPB_DMA_VALID);

	return 0;

free_tx_spb:
	dma_free_coherent(kdev, DESC_RING_SIZE, intf->tx_spb_cpu,
			  intf->tx_spb_dma_addr);

	return ret;
}

static int bcmasp_xbar_open(struct net_device *dev)
{
	struct bcmasp_intf *intf = netdev_priv(dev);
	int ret;

	netif_dbg(intf, ifup, dev, "%s\n", __func__);

	/* We are dependent on the upper interface to set-up clocking and other
	 * hardware resources for us.
	 */
	if (!(intf->lowerdev->flags & IFF_UP))
		return -ENETDOWN;

	ret = bcmasp_xbar_init_tx(intf);
	if (ret)
		return ret;

	ret = bcmasp_xbar_init_rx(intf);
	if (ret)
		goto err_reclaim_tx;

	/* Turn on asp */
	bcmasp_xbar_enable_tx(intf, 1);
	bcmasp_xbar_enable_rx(intf, 1);

	napi_enable(&intf->tx_napi);
	napi_enable(&intf->rx_napi);

	bcmasp_enable_rx_irq(intf, 1);
	bcmasp_enable_tx_irq(intf, 1);

	netif_start_queue(dev);
	netif_carrier_on(dev);

	return 0;

err_reclaim_tx:
	bcmasp_reclaim_free_all_tx(intf);
	return ret;
}

static int bcmasp_xbar_stop(struct net_device *dev)
{
	struct bcmasp_intf *intf = netdev_priv(dev);
	u32 reg, timeout = 1000;

	netif_dbg(intf, ifdown, dev, "%s\n", __func__);

	netif_carrier_off(dev);

	/* Stop tx from updating HW */
	netif_tx_disable(dev);

	napi_disable(&intf->tx_napi);

	bcmasp_xbar_enable_tx(intf, 0);

	/* Flush any trasnmitted packets in the pipe */
	rx_spb_dma_wl(intf, RX_SPB_DMA_FIFO_FLUSH, RX_SPB_DMA_FIFO_CTRL);
	do {
		reg = rx_spb_dma_rl(intf, RX_SPB_DMA_FIFO_STATUS);
		if (!(reg & RX_SPB_DMA_FIFO_FLUSH))
			break;
		usleep_range(1000, 2000);
	} while (timeout-- > 0);
	rx_spb_dma_wl(intf, 0, RX_SPB_DMA_FIFO_CTRL);

	bcmasp_xbar_enable_rx(intf, 0);

	napi_disable(&intf->rx_napi);

	/* Disable interrupts */
	bcmasp_enable_tx_irq(intf, 0);
	bcmasp_enable_rx_irq(intf, 0);

	netif_napi_del(&intf->tx_napi);
	netif_napi_del(&intf->rx_napi);

	bcmasp_reclaim_free_all_rx(intf);
	bcmasp_reclaim_free_all_tx(intf);

	return 0;
}

static inline unsigned long bcmasp_outdma_desc_rq(struct bcmasp_intf *intf)
{
	return outdma_rq(intf, OUTDMA_DESC_DMA_VALID);
}

static inline void bcmasp_outdma_buffer_wq(struct bcmasp_intf *intf,
					   dma_addr_t addr)
{
	outdma_wq(intf, addr, OUTDMA_DATA_DMA_READ);
}

static inline bool bcmasp_outdma_buffer_rdy(struct bcmasp_intf *intf,
					    dma_addr_t end_addr)
{
	unsigned long data_valid;

	data_valid = outdma_rq(intf, OUTDMA_DATA_DMA_VALID) + 1;
	if (data_valid == intf->rx_ring_dma + RING_BUFFER_SIZE)
		data_valid = intf->rx_ring_dma;

	/* empty, no data is ready */
	if (data_valid == intf->rx_ring_dma_read)
		return false;

	/* Check if the end addr is within the ready data */
	/* No wrap case */
	if ((data_valid > intf->rx_ring_dma_read) &&
	    (end_addr <= data_valid) &&
	    (end_addr > intf->rx_ring_dma_read))
		return true;

	/* Wrap case */
	if ((data_valid < intf->rx_ring_dma_read) &&
	    ((end_addr <= data_valid) ||
	    (end_addr > intf->rx_ring_dma_read)))
		return true;

	return false;
}

static inline void bcmasp_outdma_desc_wq(struct bcmasp_intf *intf,
					 dma_addr_t addr)
{
	outdma_wq(intf, addr, OUTDMA_DESC_DMA_READ);
}

static inline unsigned long bcmasp_rx_spb_dma_rq(struct bcmasp_intf *intf)
{
	return rx_spb_dma_rq(intf, RX_SPB_DMA_READ);
}

static inline void bcmasp_rx_spb_dma_wq(struct bcmasp_intf *intf,
					dma_addr_t addr)
{
	rx_spb_dma_wq(intf, addr, RX_SPB_DMA_VALID);
}

static const struct bcmasp_intf_ops bcmasp_xbar_intf_ops = {
	.rx_desc_read = bcmasp_outdma_desc_rq,
	.rx_buffer_write = bcmasp_outdma_buffer_wq,
	.rx_buffer_rdy = bcmasp_outdma_buffer_rdy,
	.rx_desc_write = bcmasp_outdma_desc_wq,
	.tx_read = bcmasp_rx_spb_dma_rq,
	.tx_write = bcmasp_rx_spb_dma_wq,
};

static void bcmasp_xbar_tx_timeout(struct net_device *dev, unsigned int txq)
{
	struct bcmasp_intf *intf = netdev_priv(dev);

	netif_dbg(intf, tx_err, dev, "transmit timeout!\n");

	netif_trans_update(dev);
	dev->stats.tx_errors++;

	netif_wake_queue(dev);
}

static void bcmasp_xbar_set_rx_mode(struct net_device *dev)
{
	struct bcmasp_intf *intf = netdev_priv(dev);
	struct net_device *lower = intf->lowerdev;

	dev_mc_sync(lower, dev);
	dev_uc_sync(lower, dev);
}

static int bcmasp_xbar_set_mac_address(struct net_device *dev, void *a)
{
	struct bcmasp_intf *intf = netdev_priv(dev);
	struct net_device *lower = intf->lowerdev;
	struct sockaddr *addr = a;
	int err;

	if (!is_valid_ether_addr(addr->sa_data))
		return -EADDRNOTAVAIL;

	if (!(dev->flags & IFF_UP))
		goto out;

	if (!ether_addr_equal(addr->sa_data, lower->dev_addr)) {
		err = dev_uc_add(lower, addr->sa_data);
		if (err < 0)
			return err;
	}

	if (!ether_addr_equal(dev->dev_addr, lower->dev_addr))
		dev_uc_del(lower, dev->dev_addr);

out:
	ether_addr_copy(dev->dev_addr, addr->sa_data);

	return 0;
}

static struct net_device_stats *bcmasp_xbar_get_stats(struct net_device *dev)
{
	return &dev->stats;
}

static int bcmasp_xbar_get_iflink(const struct net_device *dev)
{
	struct bcmasp_intf *intf = netdev_priv(dev);

	return intf->lowerdev->ifindex;
}

static void bcmasp_xbar_get_drvinfo(struct net_device *dev,
				    struct ethtool_drvinfo *info)
{
	strlcpy(info->driver, "bcmasp", sizeof(info->driver));
	strlcpy(info->version, "v2.0", sizeof(info->version));
}

static u32 bcmasp_xbar_get_msglevel(struct net_device *dev)
{
	struct bcmasp_intf *intf = netdev_priv(dev);

	return intf->msg_enable;
}

static void bcmasp_xbar_set_msglevel(struct net_device *dev, u32 level)
{
	struct bcmasp_intf *intf = netdev_priv(dev);

	intf->msg_enable = level;
}

static const struct ethtool_ops bcmasp_xbar_ethtool_ops = {
	.get_drvinfo		= bcmasp_xbar_get_drvinfo,
	.get_msglevel		= bcmasp_xbar_get_msglevel,
	.set_msglevel		= bcmasp_xbar_set_msglevel,
};

#define BCMASP_XBAR_KIND	"bcmasp-xbar"

static struct device_type bcmasp_xbar_type = {
	.name			= BCMASP_XBAR_KIND,
};

static const struct net_device_ops bcmasp_xbar_netdev_ops = {
	.ndo_open		= bcmasp_xbar_open,
	.ndo_stop		= bcmasp_xbar_stop,
	.ndo_start_xmit		= bcmasp_xmit,
	.ndo_tx_timeout		= bcmasp_xbar_tx_timeout,
	.ndo_set_rx_mode	= bcmasp_xbar_set_rx_mode,
	.ndo_set_mac_address	= bcmasp_xbar_set_mac_address,
	.ndo_get_stats		= bcmasp_xbar_get_stats,
	.ndo_get_iflink		= bcmasp_xbar_get_iflink,
};

static void bcmasp_xbar_setup(struct net_device *dev)
{
	ether_setup(dev);

	dev->netdev_ops = &bcmasp_xbar_netdev_ops;
	dev->ethtool_ops = &bcmasp_xbar_ethtool_ops;
	dev->features |= NETIF_F_IP_CSUM | NETIF_F_IPV6_CSUM | NETIF_F_SG;
	dev->hw_features |= dev->features;
	dev->needs_free_netdev = true;
}

static void bcmasp_xbar_setup_ll(struct net_device *dev, struct net_device *lowerdev)
{
	struct bcmasp_intf *intf = netdev_priv(dev);
	struct bcmasp_intf *lower_intf;
	struct bcmasp_priv *priv;

	intf->lowerdev = lowerdev;
	intf->ndev = dev;
	intf->port = ASP_PORT_INVALID;
	intf->ops = &bcmasp_xbar_intf_ops;

	/* First two upper ports are reserved for unimac ports so the mapping goes
	 * port0: intf #0 <=> no xbar
	 * port1: intf #1 <=> no xbar
	 * port2: intf #2 <=> xbar #0
	 * port3: intf #3 <=> xbar #1
	 * ...
	 */
	lower_intf = netdev_priv(lowerdev);
	priv = lower_intf->parent;
	intf->channel = ASP_SPB_RX_CHANNEL_START + lower_intf->port - 2;
	intf->parent = priv;
	/* Insert into xbar intf array */
	priv->xbars[priv->xbar_inited_count++] = intf;
	SET_NETDEV_DEV(dev, &lower_intf->parent->pdev->dev);
	SET_NETDEV_DEVTYPE(dev, &bcmasp_xbar_type);

	/* Map our resources, fast-path I/O and then slow path configuration */
	intf->rx_spb_dma = priv->base + RX_SPB_DMA_OFFSET(intf);
	intf->outdma = priv->base + OUTDMA_OFFSET(intf);
	intf->res.rx_spb_ctrl = priv->base + RX_SPB_CTRL_OFFSET(intf);
	if (priv->legacy) {
		intf->res.rx_pause_ctrl = priv->base +
			RX_PAUSE_CTRL_OFFSET_LEGACY(intf);
	} else {
		intf->res.rx_pause_ctrl = priv->base + RX_PAUSE_CTRL_OFFSET(intf);
	}
	intf->res.rx_spb_top = priv->base + RX_SPB_TOP_CTRL_OFFSET(intf);

	intf->msg_enable = netif_msg_init(-1, NETIF_MSG_DRV |
					  NETIF_MSG_PROBE |
					  NETIF_MSG_LINK);
}

static int bcmasp_xbar_newlink(struct net *src_net, struct net_device *dev,
			       struct nlattr *tb[], struct nlattr *data[],
			       struct netlink_ext_ack *extack)
{
	struct net_device *lowerdev;
	struct bcmasp_intf *intf;
	int err;

	if (!tb[IFLA_LINK]) {
		NL_SET_ERR_MSG_MOD(extack, "Invalid link parameter");
		err = -EINVAL;
		goto out;
	}

	lowerdev = __dev_get_by_index(src_net, nla_get_u32(tb[IFLA_LINK]));
	if (!lowerdev) {
		NL_SET_ERR_MSG_MOD(extack, "Invalid lower device");
		err = -EINVAL;
		goto out;
	}

	if (!tb[IFLA_ADDRESS])
		eth_hw_addr_random(dev);

	dev_hold(lowerdev);
	intf = netdev_priv(lowerdev);
	/* We cannot attach a crossbar to a standard Ethernet port */
	if (bcmasp_intf_has_umac(intf)) {
		NL_SET_ERR_MSG_MOD(extack, "Unable to link to UniMAC interface");
		err = -EINVAL;
		goto out_dev_put;
	}
	bcmasp_xbar_setup_ll(dev, lowerdev);

	netif_carrier_off(dev);

	err = register_netdevice(dev);
	if (err)
		goto out_dev_put;

	err = netdev_upper_dev_link(lowerdev, dev, extack);
	if (err)
		goto unregister_netdev;

	return err;

unregister_netdev:
	unregister_netdevice(dev);
out_dev_put:
	dev_put(lowerdev);
out:
	return err;
}

static void bcmasp_xbar_dellink(struct net_device *dev,
				struct list_head *head)
{
	struct bcmasp_intf *intf = netdev_priv(dev);
	struct net_device *lowerdev = intf->lowerdev;
	struct bcmasp_priv *priv = intf->parent;

	/* Upper can now go away */
	netdev_upper_dev_unlink(lowerdev, dev);
	unregister_netdevice_queue(dev, head);
	/* Decrease the number of initialized xbars */
	priv->xbar_inited_count--;
	dev_put(lowerdev);
}

static struct rtnl_link_ops bcmasp_xbar_rtnl_ops = {
	.kind		= BCMASP_XBAR_KIND,
	.setup		= bcmasp_xbar_setup,
	.newlink	= bcmasp_xbar_newlink,
	.dellink	= bcmasp_xbar_dellink,
	.priv_size	= sizeof(struct bcmasp_intf),
};

static int bcmasp_xbar_device_event(struct notifier_block *unused,
				    unsigned long event, void *ptr)
{
	struct net_device *dev = netdev_notifier_info_to_dev(ptr);
	struct bcmasp_intf *intf = NULL;
	struct bcmasp_priv *priv = NULL;
	unsigned int i = 0;
	LIST_HEAD(list);
	int err = 0;

	/* We have no actions targeting the xbar interface itself yet */
	if (dev->netdev_ops == &bcmasp_xbar_netdev_ops)
		goto out;

	/* If this is a foreign interface, we also do not have anything to do */
	if (!netdev_is_bcmasp_intf(dev))
		goto out;

	intf = netdev_priv(dev);
	priv = intf->parent;

	switch (event) {
	case NETDEV_UNREGISTER:
		if (dev->reg_state != NETREG_UNREGISTERING)
			break;

		/* Schedule xbars for unregistration */
		for (i = 0; i < priv->xbar_inited_count; i++)
			bcmasp_xbar_dellink(priv->xbars[i]->ndev, &list);

		unregister_netdevice_many(&list);
		break;
	default:
		break;
	}
out:
	return notifier_from_errno(err);
}

static struct notifier_block bcmasp_xbar_notifier __read_mostly = {
	.notifier_call	= bcmasp_xbar_device_event,
};

static int __init bcmasp_xbar_init(void)
{
	int err;

	err = register_netdevice_notifier(&bcmasp_xbar_notifier);
	if (err)
		return err;

	err = rtnl_link_register(&bcmasp_xbar_rtnl_ops);
	if (err)
		unregister_netdevice_notifier(&bcmasp_xbar_notifier);
	return err;
}
module_init(bcmasp_xbar_init);

static void __exit bcmasp_xbar_exit(void)
{
	rtnl_link_unregister(&bcmasp_xbar_rtnl_ops);
	unregister_netdevice_notifier(&bcmasp_xbar_notifier);
}
module_exit(bcmasp_xbar_exit);

MODULE_AUTHOR("Broadcom");
MODULE_DESCRIPTION("Broadcom ASP cross-bar interface");
MODULE_LICENSE("GPL v2");
MODULE_ALIAS_RTNL_LINK(BCMASP_XBAR_KIND);
