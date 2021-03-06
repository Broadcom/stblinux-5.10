// SPDX-License-Identifier: GPL-2.0+
/* Copyright (C) 2009 - 2019 Broadcom */

#include <linux/bitfield.h>
#include <linux/bitops.h>
#include <linux/clk.h>
#include <linux/compiler.h>
#include <linux/delay.h>
#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/io.h>
#include <linux/ioport.h>
#include <linux/irqchip/chained_irq.h>
#include <linux/irqdomain.h>
#include <linux/kdebug.h>
#include <linux/kernel.h>
#include <linux/list.h>
#include <linux/log2.h>
#include <linux/module.h>
#include <linux/msi.h>
#include <linux/notifier.h>
#include <linux/of_address.h>
#include <linux/of_irq.h>
#include <linux/of_pci.h>
#include <linux/of_platform.h>
#include <linux/pci.h>
#include <linux/printk.h>
#include <linux/regulator/consumer.h>
#include <linux/reset.h>
#include <linux/sizes.h>
#include <linux/slab.h>
#include <linux/string.h>
#include <linux/types.h>

#include "../pci.h"

/* BRCM_PCIE_CAP_REGS - Offset for the mandatory capability config regs */
#define BRCM_PCIE_CAP_REGS				0x00ac

/* Broadcom STB PCIe Register Offsets */
#define PCIE_RC_CFG_VENDOR_VENDOR_SPECIFIC_REG1				0x0188
#define  PCIE_RC_CFG_VENDOR_VENDOR_SPECIFIC_REG1_ENDIAN_MODE_BAR2_MASK	0xc
#define  PCIE_RC_CFG_VENDOR_SPCIFIC_REG1_LITTLE_ENDIAN			0x0

#define PCIE_RC_CFG_PRIV1_ID_VAL3			0x043c
#define  PCIE_RC_CFG_PRIV1_ID_VAL3_CLASS_CODE_MASK	0xffffff

#define PCIE_RC_CFG_PRIV1_LINK_CAPABILITY			0x04dc
#define  PCIE_RC_CFG_PRIV1_LINK_CAPABILITY_ASPM_SUPPORT_MASK	0xc00

#define PCIE_RC_DL_MDIO_ADDR				0x1100
#define PCIE_RC_DL_MDIO_WR_DATA				0x1104
#define PCIE_RC_DL_MDIO_RD_DATA				0x1108

#define PCIE_MISC_MISC_CTRL				0x4008
#define  PCIE_MISC_MISC_CTRL_SCB_ACCESS_EN_MASK		0x1000
#define  PCIE_MISC_MISC_CTRL_CFG_READ_UR_MODE_MASK	0x2000
#define  PCIE_MISC_MISC_CTRL_MAX_BURST_SIZE_MASK	0x300000

#define  PCIE_MISC_MISC_CTRL_SCB0_SIZE_MASK		0xf8000000
#define  PCIE_MISC_MISC_CTRL_SCB1_SIZE_MASK		0x07c00000
#define  PCIE_MISC_MISC_CTRL_SCB2_SIZE_MASK		0x0000001f
#define  SCB_SIZE_MASK(x) PCIE_MISC_MISC_CTRL_SCB ## x ## _SIZE_MASK

#define PCIE_MISC_CPU_2_PCIE_MEM_WIN0_LO		0x400c
#define PCIE_MEM_WIN0_LO(win)	\
		PCIE_MISC_CPU_2_PCIE_MEM_WIN0_LO + ((win) * 8)

#define PCIE_MISC_CPU_2_PCIE_MEM_WIN0_HI		0x4010
#define PCIE_MEM_WIN0_HI(win)	\
		PCIE_MISC_CPU_2_PCIE_MEM_WIN0_HI + ((win) * 8)

#define PCIE_MISC_RC_BAR1_CONFIG_LO			0x402c
#define  PCIE_MISC_RC_BAR1_CONFIG_LO_SIZE_MASK		0x1f

#define PCIE_MISC_RC_BAR2_CONFIG_LO			0x4034
#define  PCIE_MISC_RC_BAR2_CONFIG_LO_SIZE_MASK		0x1f
#define PCIE_MISC_RC_BAR2_CONFIG_HI			0x4038

#define PCIE_MISC_RC_BAR3_CONFIG_LO			0x403c
#define  PCIE_MISC_RC_BAR3_CONFIG_LO_SIZE_MASK		0x1f

#define PCIE_MISC_MSI_BAR_CONFIG_LO			0x4044
#define PCIE_MISC_MSI_BAR_CONFIG_HI			0x4048

#define PCIE_MISC_MSI_DATA_CONFIG			0x404c
#define  PCIE_MISC_MSI_DATA_CONFIG_VAL_32		0xffe06540
#define  PCIE_MISC_MSI_DATA_CONFIG_VAL_8		0xfff86540

#define PCIE_MISC_PCIE_CTRL				0x4064
#define  PCIE_MISC_PCIE_CTRL_PCIE_L23_REQUEST_MASK	0x1
#define PCIE_MISC_PCIE_CTRL_PCIE_PERSTB_MASK		0x4

#define PCIE_MISC_PCIE_STATUS				0x4068
#define  PCIE_MISC_PCIE_STATUS_PCIE_PORT_MASK		0x80
#define  PCIE_MISC_PCIE_STATUS_PCIE_DL_ACTIVE_MASK	0x20
#define  PCIE_MISC_PCIE_STATUS_PCIE_PHYLINKUP_MASK	0x10
#define  PCIE_MISC_PCIE_STATUS_PCIE_LINK_IN_L23_MASK	0x40

#define PCIE_MISC_REVISION				0x406c
#define  BRCM_PCIE_HW_REV_33				0x0303

#define PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT		0x4070
#define  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT_LIMIT_MASK	0xfff00000
#define  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT_BASE_MASK	0xfff0
#define PCIE_MEM_WIN0_BASE_LIMIT(win)	\
		PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT + ((win) * 4)

#define PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_HI			0x4080
#define  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_HI_BASE_MASK	0xff
#define PCIE_MEM_WIN0_BASE_HI(win)	\
		PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_HI + ((win) * 8)

#define PCIE_MISC_CPU_2_PCIE_MEM_WIN0_LIMIT_HI			0x4084
#define  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_LIMIT_HI_LIMIT_MASK	0xff
#define PCIE_MEM_WIN0_LIMIT_HI(win)	\
		PCIE_MISC_CPU_2_PCIE_MEM_WIN0_LIMIT_HI + ((win) * 8)

#define  PCIE_MISC_HARD_PCIE_HARD_DEBUG_CLKREQ_DEBUG_ENABLE_MASK	0x2
#define  PCIE_MISC_HARD_PCIE_HARD_DEBUG_SERDES_IDDQ_MASK		0x08000000

#define PCIE_MISC_UBUS_BAR2_CONFIG_REMAP			0x40b4
#define  PCIE_MISC_UBUS_BAR2_CONFIG_REMAP_ACCESS_EN_MASK	0x1

#define PCIE_MSI_INTR2_BASE		0x4500
/* Offsets from PCIE_INTR2_CPU_BASE and PCIE_MSI_INTR2_BASE */
#define  MSI_INT_STATUS			0x0
#define  MSI_INT_CLR			0x8
#define  MSI_INT_MASK_SET		0x10
#define  MSI_INT_MASK_CLR		0x14

#define PCIE_EXT_CFG_DATA				0x8000

#define PCIE_EXT_CFG_INDEX				0x9000
#define  PCIE_EXT_BUSNUM_SHIFT				20
#define  PCIE_EXT_SLOT_SHIFT				15
#define  PCIE_EXT_FUNC_SHIFT				12

#define  PCIE_RGR1_SW_INIT_1_PERST_MASK			0x1
#define  PCIE_RGR1_SW_INIT_1_PERST_SHIFT		0x0

#define RGR1_SW_INIT_1_INIT_GENERIC_MASK		0x2
#define RGR1_SW_INIT_1_INIT_GENERIC_SHIFT		0x1
#define RGR1_SW_INIT_1_INIT_7278_MASK			0x1
#define RGR1_SW_INIT_1_INIT_7278_SHIFT			0x0

/* PCIe parameters */
#define BRCM_NUM_PCIE_OUT_WINS		0x4
#define BRCM_INT_PCI_MSI_NR		32
#define BRCM_INT_PCI_MSI_LEGACY_NR	8
#define BRCM_INT_PCI_MSI_SHIFT		0

/* MSI target adresses */
#define BRCM_MSI_TARGET_ADDR_LT_4GB	0x0fffffffcULL
#define BRCM_MSI_TARGET_ADDR_GT_4GB	0xffffffffcULL

/* MDIO registers */
#define MDIO_PORT0			0x0
#define MDIO_DATA_MASK			0x7fffffff
#define MDIO_PORT_MASK			0xf0000
#define MDIO_PORT_EXT_MASK		0x200000
#define MDIO_REGAD_MASK			0xffff
#define MDIO_CMD_MASK			0x00100000
#define MDIO_CMD_READ			0x1
#define MDIO_CMD_WRITE			0x0
#define MDIO_DATA_DONE_MASK		0x80000000
#define MDIO_RD_DONE(x)			(((x) & MDIO_DATA_DONE_MASK) ? 1 : 0)
#define MDIO_WT_DONE(x)			(((x) & MDIO_DATA_DONE_MASK) ? 0 : 1)
#define SSC_REGS_ADDR			0x1100
#define SET_ADDR_OFFSET			0x1f
#define SSC_CNTL_OFFSET			0x2
#define SSC_CNTL_OVRD_EN_MASK		0x8000
#define SSC_CNTL_OVRD_VAL_MASK		0x4000
#define SSC_STATUS_OFFSET		0x1
#define SSC_STATUS_SSC_MASK		0x400
#define SSC_STATUS_PLL_LOCK_MASK	0x800
#define PCIE_BRCM_MAX_MEMC		3

#define IDX_ADDR(pcie)			(pcie->reg_offsets[EXT_CFG_INDEX])
#define DATA_ADDR(pcie)			(pcie->reg_offsets[EXT_CFG_DATA])
#define PCIE_RGR1_SW_INIT_1(pcie)	(pcie->reg_offsets[RGR1_SW_INIT_1])
#define	HARD_DEBUG(pcie)		(pcie->reg_offsets[PCIE_HARD_DEBUG])
#define	INTR2_CPU_BASE(pcie)		(pcie->reg_offsets[PCIE_INTR2_CPU_BASE])

/* Rescal registers */
#define PCIE_DVT_PMU_PCIE_PHY_CTRL				0xc700
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_NFLDS			0x3
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_DIG_RESET_MASK		0x4
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_DIG_RESET_SHIFT	0x2
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_RESET_MASK		0x2
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_RESET_SHIFT		0x1
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_PWRDN_MASK		0x1
#define  PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_PWRDN_SHIFT		0x0

/* Error report regiseters */
#define PCIE_OUTB_ERR_TREAT				0x6000
#define  PCIE_OUTB_ERR_TREAT_CONFIG_MASK		0x1
#define  PCIE_OUTB_ERR_TREAT_MEM_MASK			0x2
#define PCIE_OUTB_ERR_VALID				0x6004
#define PCIE_OUTB_ERR_CLEAR				0x6008
#define PCIE_OUTB_ERR_ACC_INFO				0x600c
#define  PCIE_OUTB_ERR_ACC_INFO_CFG_ERR_MASK		0x01
#define  PCIE_OUTB_ERR_ACC_INFO_MEM_ERR_MASK		0x02
#define  PCIE_OUTB_ERR_ACC_INFO_TYPE_64_MASK		0x04
#define  PCIE_OUTB_ERR_ACC_INFO_DIR_WRITE_MASK		0x10
#define  PCIE_OUTB_ERR_ACC_INFO_BYTE_LANES_MASK		0xff00
#define PCIE_OUTB_ERR_ACC_ADDR				0x6010
#define PCIE_OUTB_ERR_ACC_ADDR_BUS_MASK			0xff00000
#define PCIE_OUTB_ERR_ACC_ADDR_DEV_MASK			0xf8000
#define PCIE_OUTB_ERR_ACC_ADDR_FUNC_MASK		0x7000
#define PCIE_OUTB_ERR_ACC_ADDR_REG_MASK			0xfff
#define PCIE_OUTB_ERR_CFG_CAUSE				0x6014
#define  PCIE_OUTB_ERR_CFG_CAUSE_TIMEOUT_MASK		0x40
#define  PCIE_OUTB_ERR_CFG_CAUSE_ABORT_MASK		0x20
#define  PCIE_OUTB_ERR_CFG_CAUSE_UNSUPP_REQ_MASK	0x10
#define  PCIE_OUTB_ERR_CFG_CAUSE_ACC_TIMEOUT_MASK	0x4
#define  PCIE_OUTB_ERR_CFG_CAUSE_ACC_DISABLED_MASK	0x2
#define  PCIE_OUTB_ERR_CFG_CAUSE_ACC_64BIT__MASK	0x1
#define PCIE_OUTB_ERR_MEM_ADDR_LO			0x6018
#define PCIE_OUTB_ERR_MEM_ADDR_HI			0x601c
#define PCIE_OUTB_ERR_MEM_CAUSE				0x6020
#define  PCIE_OUTB_ERR_MEM_CAUSE_TIMEOUT_MASK		0x40
#define  PCIE_OUTB_ERR_MEM_CAUSE_ABORT_MASK		0x20
#define  PCIE_OUTB_ERR_MEM_CAUSE_UNSUPP_REQ_MASK	0x10
#define  PCIE_OUTB_ERR_MEM_CAUSE_ACC_DISABLED_MASK	0x2
#define  PCIE_OUTB_ERR_MEM_CAUSE_BAD_ADDR_MASK		0x1

/* Forward declarations */
struct brcm_pcie;
static inline void brcm_pcie_bridge_sw_init_set_7278(struct brcm_pcie *pcie, u32 val);
static inline void brcm_pcie_bridge_sw_init_set_generic(struct brcm_pcie *pcie, u32 val);
static inline void brcm_pcie_perst_set_7278(struct brcm_pcie *pcie, u32 val);
static inline void brcm_pcie_perst_set_generic(struct brcm_pcie *pcie, u32 val);

enum {
	RGR1_SW_INIT_1,
	EXT_CFG_INDEX,
	EXT_CFG_DATA,
	PCIE_HARD_DEBUG,
	PCIE_INTR2_CPU_BASE,
};

enum {
	RGR1_SW_INIT_1_INIT_MASK,
	RGR1_SW_INIT_1_INIT_SHIFT,
};

enum pcie_type {
	GENERIC,
	BCM7278,
	BCM2711,
	BCM2712,
};

struct pcie_cfg_data {
	const int *offsets;
	const enum pcie_type type;
	void (*perst_set)(struct brcm_pcie *pcie, u32 val);
	void (*bridge_sw_init_set)(struct brcm_pcie *pcie, u32 val);
	const bool has_err_report;
	const bool has_phy;
};

static const int pcie_offsets[] = {
	[RGR1_SW_INIT_1] = 0x9210,
	[EXT_CFG_INDEX]  = 0x9000,
	[EXT_CFG_DATA]   = 0x9004,
	[PCIE_HARD_DEBUG] = 0x4204,
	[PCIE_INTR2_CPU_BASE] = 0x4300,
};

static const int pcie_offset_bcm2712[] = {
	[RGR1_SW_INIT_1] = 0x9210,
	[EXT_CFG_INDEX]  = 0x9000,
	[EXT_CFG_DATA]   = 0x9004,
	[PCIE_HARD_DEBUG] = 0x4304,
	[PCIE_INTR2_CPU_BASE] = 0x4400,
};

static const struct pcie_cfg_data generic_cfg = {
	.offsets	= pcie_offsets,
	.type		= GENERIC,
	.perst_set	= brcm_pcie_perst_set_generic,
	.bridge_sw_init_set = brcm_pcie_bridge_sw_init_set_generic,
};

static const int pcie_offset_bcm7278[] = {
	[RGR1_SW_INIT_1] = 0xc010,
	[EXT_CFG_INDEX] = 0x9000,
	[EXT_CFG_DATA] = 0x9004,
	[PCIE_HARD_DEBUG] = 0x4204,
	[PCIE_INTR2_CPU_BASE] = 0x4300,
};

static const struct pcie_cfg_data bcm7278_cfg = {
	.offsets	= pcie_offset_bcm7278,
	.type		= BCM7278,
	.perst_set	= brcm_pcie_perst_set_7278,
	.bridge_sw_init_set = brcm_pcie_bridge_sw_init_set_7278,
};

static const struct pcie_cfg_data bcm2711_cfg = {
	.offsets	= pcie_offsets,
	.type		= BCM2711,
	.perst_set	= brcm_pcie_perst_set_generic,
	.bridge_sw_init_set = brcm_pcie_bridge_sw_init_set_generic,
};

static const struct pcie_cfg_data bcm7216_cfg = {
	.offsets	= pcie_offset_bcm7278,
	.type		= BCM7278,
	.perst_set	= brcm_pcie_perst_set_7278,
	.bridge_sw_init_set = brcm_pcie_bridge_sw_init_set_7278,
	.has_err_report = true,
	.has_phy	= true,
};

static const struct pcie_cfg_data bcm2712_cfg = {
	.offsets	= pcie_offset_bcm2712,
	.perst_set	= brcm_pcie_perst_set_7278,
	.bridge_sw_init_set = brcm_pcie_bridge_sw_init_set_generic,
	.type		= BCM2712,
};

struct brcm_dev_pwr_supply {
	struct list_head node;
	char name[32];
	struct regulator *regulator;
};

struct brcm_msi {
	struct device		*dev;
	void __iomem		*base;
	struct device_node	*np;
	struct irq_domain	*msi_domain;
	struct irq_domain	*inner_domain;
	struct mutex		lock; /* guards the alloc/free operations */
	u64			target_addr;
	int			irq;
	/* used indicates which MSI interrupts have been alloc'd */
	unsigned long		used;
	bool			legacy;
	/* Some chips have MSIs in bits [31..24] of a shared register. */
	int			legacy_shift;
	int			nr; /* No. of MSI available, depends on chip */
	/* This is the base pointer for interrupt status/set/clr regs */
	void __iomem		*intr_base;
};

/* Internal PCIe Host Controller Information.*/
struct brcm_pcie {
	struct device		*dev;
	void __iomem		*base;
	struct clk		*clk;
	struct device_node	*np;
	bool			ssc;
	int			gen;
	u64			msi_target_addr;
	struct brcm_msi		*msi;
	const int		*reg_offsets;
	enum pcie_type		type;
	struct reset_control	*rescal;
	struct reset_control	*bridge;
	struct reset_control	*swinit;
	int			num_memc;
	u64			memc_size[PCIE_BRCM_MAX_MEMC];
	u32			hw_rev;
	void			(*perst_set)(struct brcm_pcie *pcie, u32 val);
	void			(*bridge_sw_init_set)(struct brcm_pcie *pcie, u32 val);
	bool			has_err_report;
	struct notifier_block	die_notifier;
	struct list_head	pwr_supplies;
	bool			ep_wakeup_capable;
	bool			has_phy;
};

/* Dump out PCIe errors on die or panic */
static int dump_pcie_error(struct notifier_block *self, unsigned long v, void *p)
{
	const struct brcm_pcie *pcie = container_of(self, struct brcm_pcie, die_notifier);
	void __iomem *base = pcie->base;
	int i, is_cfg_err, is_mem_err, lanes;
	char *width_str, *direction_str, lanes_str[9];
	u32 info;

	if (readl(base + PCIE_OUTB_ERR_VALID) == 0)
		return NOTIFY_DONE;
	info = readl(base + PCIE_OUTB_ERR_ACC_INFO);


	is_cfg_err = !!(info & PCIE_OUTB_ERR_ACC_INFO_CFG_ERR_MASK);
	is_mem_err = !!(info & PCIE_OUTB_ERR_ACC_INFO_MEM_ERR_MASK);
	width_str = (info & PCIE_OUTB_ERR_ACC_INFO_TYPE_64_MASK) ? "64bit" : "32bit";
	direction_str = (info & PCIE_OUTB_ERR_ACC_INFO_DIR_WRITE_MASK) ? "Write" : "Read";
	lanes = FIELD_GET(PCIE_OUTB_ERR_ACC_INFO_BYTE_LANES_MASK, info);
	for (i = 0, lanes_str[8] = 0; i < 8; i++)
		lanes_str[i] = (lanes & (1 << i)) ? '1' : '0';

	if (is_cfg_err) {
		u32 cfg_addr = readl(base + PCIE_OUTB_ERR_ACC_ADDR);
		u32 cause = readl(base + PCIE_OUTB_ERR_CFG_CAUSE);
		int bus = FIELD_GET(PCIE_OUTB_ERR_ACC_ADDR_BUS_MASK, cfg_addr);
		int dev = FIELD_GET(PCIE_OUTB_ERR_ACC_ADDR_DEV_MASK, cfg_addr);
		int func = FIELD_GET(PCIE_OUTB_ERR_ACC_ADDR_FUNC_MASK, cfg_addr);
		int reg = FIELD_GET(PCIE_OUTB_ERR_ACC_ADDR_REG_MASK, cfg_addr);

		dev_err(pcie->dev, "Error: CFG Acc, %s, %s, Bus=%d, Dev=%d, Fun=%d, Reg=0x%x, lanes=%s\n",
			width_str, direction_str, bus, dev, func, reg, lanes_str);
		dev_err(pcie->dev, " Type: TO=%d Abt=%d UnsupReq=%d AccTO=%d AccDsbld=%d Acc64bit=%d\n",
			!!(cause & PCIE_OUTB_ERR_CFG_CAUSE_TIMEOUT_MASK),
			!!(cause & PCIE_OUTB_ERR_CFG_CAUSE_ABORT_MASK),
			!!(cause & PCIE_OUTB_ERR_CFG_CAUSE_UNSUPP_REQ_MASK),
			!!(cause & PCIE_OUTB_ERR_CFG_CAUSE_ACC_TIMEOUT_MASK),
			!!(cause & PCIE_OUTB_ERR_CFG_CAUSE_ACC_DISABLED_MASK),
			!!(cause & PCIE_OUTB_ERR_CFG_CAUSE_ACC_64BIT__MASK));
	}

	if (is_mem_err) {
		u32 cause = readl(base + PCIE_OUTB_ERR_MEM_CAUSE);
		u32 lo = readl(base + PCIE_OUTB_ERR_MEM_ADDR_LO);
		u32 hi = readl(base + PCIE_OUTB_ERR_MEM_ADDR_HI);
		u64 addr = ((u64)hi << 32) | (u64)lo;

		dev_err(pcie->dev, "Error: Mem Acc, %s, %s, @0x%llx, lanes=%s\n",
			width_str, direction_str, addr, lanes_str);
		dev_err(pcie->dev, " Type: TO=%d Abt=%d UnsupReq=%d AccDsble=%d BadAddr=%d\n",
			!!(cause & PCIE_OUTB_ERR_MEM_CAUSE_TIMEOUT_MASK),
			!!(cause & PCIE_OUTB_ERR_MEM_CAUSE_ABORT_MASK),
			!!(cause & PCIE_OUTB_ERR_MEM_CAUSE_UNSUPP_REQ_MASK),
			!!(cause & PCIE_OUTB_ERR_MEM_CAUSE_ACC_DISABLED_MASK),
			!!(cause & PCIE_OUTB_ERR_MEM_CAUSE_BAD_ADDR_MASK));
	}

	/* Clear the error */
	writel(1, base + PCIE_OUTB_ERR_CLEAR);

	return NOTIFY_DONE;
}

static void brcm_register_die_notifiers(struct brcm_pcie *pcie)
{
	pcie->die_notifier.notifier_call = dump_pcie_error;
	register_die_notifier(&pcie->die_notifier);
	atomic_notifier_chain_register(&panic_notifier_list, &pcie->die_notifier);
}

static void brcm_unregister_die_notifiers(struct brcm_pcie *pcie)
{
	unregister_die_notifier(&pcie->die_notifier);
	atomic_notifier_chain_unregister(&panic_notifier_list, &pcie->die_notifier);
	pcie->die_notifier.notifier_call = NULL;
}

/*
 * This is to convert the size of the inbound "BAR" region to the
 * non-linear values of PCIE_X_MISC_RC_BAR[123]_CONFIG_LO.SIZE
 */
static int brcm_pcie_encode_ibar_size(u64 size)
{
	int log2_in = ilog2(size);

	if (log2_in >= 12 && log2_in <= 15)
		/* Covers 4KB to 32KB (inclusive) */
		return (log2_in - 12) + 0x1c;
	else if (log2_in >= 16 && log2_in <= 35)
		/* Covers 64KB to 32GB, (inclusive) */
		return log2_in - 15;
	/* Something is awry so disable */
	return 0;
}

static u32 brcm_pcie_mdio_form_pkt(int port, int regad, int cmd)
{
	u32 pkt = 0;

	pkt |= FIELD_PREP(MDIO_PORT_EXT_MASK, port >> 4);
	pkt |= FIELD_PREP(MDIO_PORT_MASK, port);
	pkt |= FIELD_PREP(MDIO_REGAD_MASK, regad);
	pkt |= FIELD_PREP(MDIO_CMD_MASK, cmd);

	return pkt;
}

/* negative return value indicates error */
static int brcm_pcie_mdio_read(void __iomem *base, u8 port, u8 regad, u32 *val)
{
	int tries;
	u32 data;

	writel(brcm_pcie_mdio_form_pkt(port, regad, MDIO_CMD_READ),
		   base + PCIE_RC_DL_MDIO_ADDR);
	readl(base + PCIE_RC_DL_MDIO_ADDR);

	data = readl(base + PCIE_RC_DL_MDIO_RD_DATA);
	for (tries = 0; !MDIO_RD_DONE(data) && tries < 10; tries++) {
		udelay(10);
		data = readl(base + PCIE_RC_DL_MDIO_RD_DATA);
	}

	*val = FIELD_GET(MDIO_DATA_MASK, data);
	return MDIO_RD_DONE(data) ? 0 : -EIO;
}

/* negative return value indicates error */
static int brcm_pcie_mdio_write(void __iomem *base, u8 port,
				u8 regad, u16 wrdata)
{
	int tries;
	u32 data;

	writel(brcm_pcie_mdio_form_pkt(port, regad, MDIO_CMD_WRITE),
		   base + PCIE_RC_DL_MDIO_ADDR);
	readl(base + PCIE_RC_DL_MDIO_ADDR);
	writel(MDIO_DATA_DONE_MASK | wrdata, base + PCIE_RC_DL_MDIO_WR_DATA);

	data = readl(base + PCIE_RC_DL_MDIO_WR_DATA);
	for (tries = 0; !MDIO_WT_DONE(data) && tries < 10; tries++) {
		udelay(10);
		data = readl(base + PCIE_RC_DL_MDIO_WR_DATA);
	}

	return MDIO_WT_DONE(data) ? 0 : -EIO;
}

/*
 * Configures device for Spread Spectrum Clocking (SSC) mode; a negative
 * return value indicates error.
 */
static int brcm_pcie_set_ssc(struct brcm_pcie *pcie)
{
	int pll, ssc;
	int ret;
	u32 tmp;

	ret = brcm_pcie_mdio_write(pcie->base, MDIO_PORT0, SET_ADDR_OFFSET,
				   SSC_REGS_ADDR);
	if (ret < 0)
		return ret;

	ret = brcm_pcie_mdio_read(pcie->base, MDIO_PORT0,
				  SSC_CNTL_OFFSET, &tmp);
	if (ret < 0)
		return ret;

	u32p_replace_bits(&tmp, 1, SSC_CNTL_OVRD_EN_MASK);
	u32p_replace_bits(&tmp, 1, SSC_CNTL_OVRD_VAL_MASK);
	ret = brcm_pcie_mdio_write(pcie->base, MDIO_PORT0,
				   SSC_CNTL_OFFSET, tmp);
	if (ret < 0)
		return ret;

	usleep_range(1000, 2000);
	ret = brcm_pcie_mdio_read(pcie->base, MDIO_PORT0,
				  SSC_STATUS_OFFSET, &tmp);
	if (ret < 0)
		return ret;

	ssc = FIELD_GET(SSC_STATUS_SSC_MASK, tmp);
	pll = FIELD_GET(SSC_STATUS_PLL_LOCK_MASK, tmp);

	return ssc && pll ? 0 : -EIO;
}

/* Limits operation to a specific generation (1, 2, or 3) */
static void brcm_pcie_set_gen(struct brcm_pcie *pcie, int gen)
{
	u16 lnkctl2 = readw(pcie->base + BRCM_PCIE_CAP_REGS + PCI_EXP_LNKCTL2);
	u32 lnkcap = readl(pcie->base + BRCM_PCIE_CAP_REGS + PCI_EXP_LNKCAP);

	lnkcap = (lnkcap & ~PCI_EXP_LNKCAP_SLS) | gen;
	writel(lnkcap, pcie->base + BRCM_PCIE_CAP_REGS + PCI_EXP_LNKCAP);

	lnkctl2 = (lnkctl2 & ~0xf) | gen;
	writew(lnkctl2, pcie->base + BRCM_PCIE_CAP_REGS + PCI_EXP_LNKCTL2);
}

static void brcm_pcie_set_outbound_win(struct brcm_pcie *pcie,
				       unsigned int win, u64 cpu_addr,
				       u64 pcie_addr, u64 size)
{
	u32 cpu_addr_mb_high, limit_addr_mb_high;
	phys_addr_t cpu_addr_mb, limit_addr_mb;
	int high_addr_shift;
	u32 tmp;

	/* Set the base of the pcie_addr window */
	writel(lower_32_bits(pcie_addr), pcie->base + PCIE_MEM_WIN0_LO(win));
	writel(upper_32_bits(pcie_addr), pcie->base + PCIE_MEM_WIN0_HI(win));

	/* Write the addr base & limit lower bits (in MBs) */
	cpu_addr_mb = cpu_addr / SZ_1M;
	limit_addr_mb = (cpu_addr + size - 1) / SZ_1M;

	tmp = readl(pcie->base + PCIE_MEM_WIN0_BASE_LIMIT(win));
	u32p_replace_bits(&tmp, cpu_addr_mb,
			  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT_BASE_MASK);
	u32p_replace_bits(&tmp, limit_addr_mb,
			  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT_LIMIT_MASK);
	writel(tmp, pcie->base + PCIE_MEM_WIN0_BASE_LIMIT(win));

	/* Write the cpu & limit addr upper bits */
	high_addr_shift =
		HWEIGHT32(PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_LIMIT_BASE_MASK);

	cpu_addr_mb_high = cpu_addr_mb >> high_addr_shift;
	tmp = readl(pcie->base + PCIE_MEM_WIN0_BASE_HI(win));
	u32p_replace_bits(&tmp, cpu_addr_mb_high,
			  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_BASE_HI_BASE_MASK);
	writel(tmp, pcie->base + PCIE_MEM_WIN0_BASE_HI(win));

	limit_addr_mb_high = limit_addr_mb >> high_addr_shift;
	tmp = readl(pcie->base + PCIE_MEM_WIN0_LIMIT_HI(win));
	u32p_replace_bits(&tmp, limit_addr_mb_high,
			  PCIE_MISC_CPU_2_PCIE_MEM_WIN0_LIMIT_HI_LIMIT_MASK);
	writel(tmp, pcie->base + PCIE_MEM_WIN0_LIMIT_HI(win));
}

static struct irq_chip brcm_msi_irq_chip = {
	.name            = "BRCM STB PCIe MSI",
	.irq_ack         = irq_chip_ack_parent,
	.irq_mask        = pci_msi_mask_irq,
	.irq_unmask      = pci_msi_unmask_irq,
};

static struct msi_domain_info brcm_msi_domain_info = {
	/* Multi MSI is supported by the controller, but not by this driver */
	.flags	= (MSI_FLAG_USE_DEF_DOM_OPS | MSI_FLAG_USE_DEF_CHIP_OPS |
		   MSI_FLAG_MULTI_PCI_MSI),
	.chip	= &brcm_msi_irq_chip,
};

static void brcm_pcie_msi_isr(struct irq_desc *desc)
{
	struct irq_chip *chip = irq_desc_get_chip(desc);
	unsigned long status, virq;
	struct brcm_msi *msi;
	struct device *dev;
	u32 bit;

	chained_irq_enter(chip, desc);
	msi = irq_desc_get_handler_data(desc);
	dev = msi->dev;

	status = readl(msi->intr_base + MSI_INT_STATUS);
	status >>= msi->legacy_shift;

	for_each_set_bit(bit, &status, msi->nr) {
		virq = irq_find_mapping(msi->inner_domain, bit);
		if (virq)
			generic_handle_irq(virq);
		else
			dev_dbg(dev, "unexpected MSI\n");
	}

	chained_irq_exit(chip, desc);
}

static void brcm_msi_compose_msi_msg(struct irq_data *data, struct msi_msg *msg)
{
	struct brcm_msi *msi = irq_data_get_irq_chip_data(data);

	msg->address_lo = lower_32_bits(msi->target_addr);
	msg->address_hi = upper_32_bits(msi->target_addr);
	msg->data = (0xffff & PCIE_MISC_MSI_DATA_CONFIG_VAL_32) | data->hwirq;
}

static int brcm_msi_set_affinity(struct irq_data *irq_data,
				 const struct cpumask *mask, bool force)
{
	return -EINVAL;
}

static void brcm_msi_ack_irq(struct irq_data *data)
{
	struct brcm_msi *msi = irq_data_get_irq_chip_data(data);
	const int shift_amt = data->hwirq + msi->legacy_shift;

	writel(1 << shift_amt, msi->intr_base + MSI_INT_CLR);
}


static struct irq_chip brcm_msi_bottom_irq_chip = {
	.name			= "BRCM STB MSI",
	.irq_compose_msi_msg	= brcm_msi_compose_msi_msg,
	.irq_set_affinity	= brcm_msi_set_affinity,
	.irq_ack                = brcm_msi_ack_irq,
};

static int brcm_msi_alloc(struct brcm_msi *msi, unsigned int nr_irqs)
{
	int hwirq;

	mutex_lock(&msi->lock);
	hwirq = bitmap_find_free_region(&msi->used, msi->nr, order_base_2(nr_irqs));
	mutex_unlock(&msi->lock);

	return hwirq;
}

static void brcm_msi_free(struct brcm_msi *msi, unsigned long hwirq,
	unsigned int nr_irqs)
{
	mutex_lock(&msi->lock);
	bitmap_release_region(&msi->used, hwirq, order_base_2(nr_irqs));
	mutex_unlock(&msi->lock);
}

static int brcm_irq_domain_alloc(struct irq_domain *domain, unsigned int virq,
				 unsigned int nr_irqs, void *args)
{
	struct brcm_msi *msi = domain->host_data;
	int hwirq, i;

	hwirq = brcm_msi_alloc(msi, nr_irqs);

	if (hwirq < 0)
		return hwirq;

	for (i = 0; i < nr_irqs; i++)
		irq_domain_set_info(domain, virq + i, hwirq + i,
				    &brcm_msi_bottom_irq_chip, domain->host_data,
				    handle_edge_irq, NULL, NULL);
	return 0;
}

static void brcm_irq_domain_free(struct irq_domain *domain,
				 unsigned int virq, unsigned int nr_irqs)
{
	struct irq_data *d = irq_domain_get_irq_data(domain, virq);
	struct brcm_msi *msi = irq_data_get_irq_chip_data(d);

	brcm_msi_free(msi, d->hwirq, nr_irqs);
}

static const struct irq_domain_ops msi_domain_ops = {
	.alloc	= brcm_irq_domain_alloc,
	.free	= brcm_irq_domain_free,
};

static int brcm_allocate_domains(struct brcm_msi *msi)
{
	struct fwnode_handle *fwnode = of_node_to_fwnode(msi->np);
	struct device *dev = msi->dev;

	msi->inner_domain = irq_domain_add_linear(NULL, msi->nr, &msi_domain_ops, msi);
	if (!msi->inner_domain) {
		dev_err(dev, "failed to create IRQ domain\n");
		return -ENOMEM;
	}

	msi->msi_domain = pci_msi_create_irq_domain(fwnode,
						    &brcm_msi_domain_info,
						    msi->inner_domain);
	if (!msi->msi_domain) {
		dev_err(dev, "failed to create MSI domain\n");
		irq_domain_remove(msi->inner_domain);
		return -ENOMEM;
	}

	return 0;
}

static void brcm_free_domains(struct brcm_msi *msi)
{
	irq_domain_remove(msi->msi_domain);
	irq_domain_remove(msi->inner_domain);
}

static void brcm_msi_remove(struct brcm_pcie *pcie)
{
	struct brcm_msi *msi = pcie->msi;

	if (!msi)
		return;
	irq_set_chained_handler(msi->irq, NULL);
	irq_set_handler_data(msi->irq, NULL);
	brcm_free_domains(msi);
}

static void brcm_msi_set_regs(struct brcm_msi *msi)
{
	u32 val = __GENMASK(31, msi->legacy_shift);

	writel(val, msi->intr_base + MSI_INT_MASK_CLR);
	writel(val, msi->intr_base + MSI_INT_CLR);

	/*
	 * The 0 bit of PCIE_MISC_MSI_BAR_CONFIG_LO is repurposed to MSI
	 * enable, which we set to 1.
	 */
	writel(lower_32_bits(msi->target_addr) | 0x1,
	       msi->base + PCIE_MISC_MSI_BAR_CONFIG_LO);
	writel(upper_32_bits(msi->target_addr),
	       msi->base + PCIE_MISC_MSI_BAR_CONFIG_HI);

	val = msi->legacy ? PCIE_MISC_MSI_DATA_CONFIG_VAL_8 : PCIE_MISC_MSI_DATA_CONFIG_VAL_32;
	writel(val, msi->base + PCIE_MISC_MSI_DATA_CONFIG);
}

static int brcm_pcie_enable_msi(struct brcm_pcie *pcie)
{
	struct brcm_msi *msi;
	int irq, ret;
	struct device *dev = pcie->dev;

	irq = irq_of_parse_and_map(dev->of_node, 1);
	if (irq <= 0) {
		dev_err(dev, "cannot map MSI interrupt\n");
		return -ENODEV;
	}

	msi = devm_kzalloc(dev, sizeof(struct brcm_msi), GFP_KERNEL);
	if (!msi)
		return -ENOMEM;

	mutex_init(&msi->lock);
	msi->dev = dev;
	msi->base = pcie->base;
	msi->np = pcie->np;
	msi->target_addr = pcie->msi_target_addr;
	msi->irq = irq;
	msi->legacy = pcie->hw_rev < BRCM_PCIE_HW_REV_33;

	if (msi->legacy) {
		msi->intr_base = msi->base + INTR2_CPU_BASE(pcie);
		msi->nr = BRCM_INT_PCI_MSI_LEGACY_NR;
		msi->legacy_shift = 24;
	} else {
		msi->intr_base = msi->base + PCIE_MSI_INTR2_BASE;
		msi->nr = BRCM_INT_PCI_MSI_NR;
		msi->legacy_shift = 0;
	}

	ret = brcm_allocate_domains(msi);
	if (ret)
		return ret;

	irq_set_chained_handler_and_data(msi->irq, brcm_pcie_msi_isr, msi);

	brcm_msi_set_regs(msi);
	pcie->msi = msi;

	return 0;
}

/* The controller is capable of serving in both RC and EP roles */
static bool brcm_pcie_rc_mode(struct brcm_pcie *pcie)
{
	void __iomem *base = pcie->base;
	u32 val = readl(base + PCIE_MISC_PCIE_STATUS);

	/* BCM7212 did this incorrectly so assume RC */
	if (pcie->type == BCM2712)
		return true;

	return !!FIELD_GET(PCIE_MISC_PCIE_STATUS_PCIE_PORT_MASK, val);
}

static bool brcm_pcie_link_up(struct brcm_pcie *pcie)
{
	u32 val = readl(pcie->base + PCIE_MISC_PCIE_STATUS);
	u32 dla = FIELD_GET(PCIE_MISC_PCIE_STATUS_PCIE_DL_ACTIVE_MASK, val);
	u32 plu = FIELD_GET(PCIE_MISC_PCIE_STATUS_PCIE_PHYLINKUP_MASK, val);

	return dla && plu;
}

/* Configuration space read/write support */
static inline int brcm_pcie_cfg_index(int busnr, int devfn, int reg)
{
	return ((PCI_SLOT(devfn) & 0x1f) << PCIE_EXT_SLOT_SHIFT)
		| ((PCI_FUNC(devfn) & 0x07) << PCIE_EXT_FUNC_SHIFT)
		| (busnr << PCIE_EXT_BUSNUM_SHIFT)
		| (reg & ~3);
}

static void __iomem *brcm_pcie_map_conf(struct pci_bus *bus, unsigned int devfn,
					int where)
{
	struct brcm_pcie *pcie = bus->sysdata;
	void __iomem *base = pcie->base;
	int idx;

	/* Accesses to the RC go right to the RC registers if slot==0 */
	if (pci_is_root_bus(bus))
		return PCI_SLOT(devfn) ? NULL : base + where;

	/* For devices, write to the config space index register */
	idx = brcm_pcie_cfg_index(bus->number, devfn, 0);
	writel(idx, pcie->base + PCIE_EXT_CFG_INDEX);
	return base + PCIE_EXT_CFG_DATA + where;
}

static struct pci_ops brcm_pcie_ops = {
	.map_bus = brcm_pcie_map_conf,
	.read = pci_generic_config_read,
	.write = pci_generic_config_write,
};

static inline void brcm_pcie_bridge_sw_init_set_generic(struct brcm_pcie *pcie, u32 val)
{
	if (pcie->bridge) {
		if (val)
			reset_control_assert(pcie->bridge);
		else
			reset_control_deassert(pcie->bridge);
	} else {
		u32 tmp, mask =  RGR1_SW_INIT_1_INIT_GENERIC_MASK;
		u32 shift = RGR1_SW_INIT_1_INIT_GENERIC_SHIFT;

		tmp = readl(pcie->base + PCIE_RGR1_SW_INIT_1(pcie));
		tmp = (tmp & ~mask) | ((val << shift) & mask);
		writel(tmp, pcie->base + PCIE_RGR1_SW_INIT_1(pcie));
	}
}

static inline void brcm_pcie_bridge_sw_init_set_7278(struct brcm_pcie *pcie, u32 val)
{
	u32 tmp, mask =  RGR1_SW_INIT_1_INIT_7278_MASK;
	u32 shift = RGR1_SW_INIT_1_INIT_7278_SHIFT;

	tmp = readl(pcie->base + PCIE_RGR1_SW_INIT_1(pcie));
	tmp = (tmp & ~mask) | ((val << shift) & mask);
	writel(tmp, pcie->base + PCIE_RGR1_SW_INIT_1(pcie));
}

static inline void brcm_pcie_perst_set_7278(struct brcm_pcie *pcie, u32 val)
{
	u32 tmp;

	tmp = readl(pcie->base + PCIE_MISC_PCIE_CTRL);
	u32p_replace_bits(&tmp, !val, PCIE_MISC_PCIE_CTRL_PCIE_PERSTB_MASK);
	writel(tmp, pcie->base +  PCIE_MISC_PCIE_CTRL);
}

static inline void brcm_pcie_perst_set_generic(struct brcm_pcie *pcie, u32 val)
{
	u32 tmp;

	tmp = readl(pcie->base + PCIE_RGR1_SW_INIT_1(pcie));
	u32p_replace_bits(&tmp, val, PCIE_RGR1_SW_INIT_1_PERST_MASK);
	writel(tmp, pcie->base + PCIE_RGR1_SW_INIT_1(pcie));
}

static inline int brcm_pcie_get_rc_bar2_size_and_offset(struct brcm_pcie *pcie,
							u64 *rc_bar2_size,
							u64 *rc_bar2_offset)
{
	struct pci_host_bridge *bridge = pci_host_bridge_from_priv(pcie);
	struct resource_entry *entry;
	struct device *dev = pcie->dev;
	u64 lowest_pcie_addr = ~(u64)0;
	int ret, i = 0;
	u64 size = 0;

	resource_list_for_each_entry(entry, &bridge->dma_ranges) {
		u64 pcie_beg = entry->res->start - entry->offset;

		size += entry->res->end - entry->res->start + 1;
		if (pcie_beg < lowest_pcie_addr)
			lowest_pcie_addr = pcie_beg;
	}

	if (lowest_pcie_addr == ~(u64)0) {
		dev_err(dev, "DT node has no dma-ranges\n");
		return -EINVAL;
	}

	ret = of_property_read_variable_u64_array(pcie->np, "brcm,scb-sizes", pcie->memc_size, 1,
						  PCIE_BRCM_MAX_MEMC);

	if (ret <= 0) {
		/* Make an educated guess */
		pcie->num_memc = 1;
		pcie->memc_size[0] = 1ULL << fls64(size - 1);
	} else {
		pcie->num_memc = ret;
	}

	/* Each memc is viewed through a "port" that is a power of 2 */
	for (i = 0, size = 0; i < pcie->num_memc; i++)
		size += pcie->memc_size[i];

	/* System memory starts at this address in PCIe-space */
	*rc_bar2_offset = lowest_pcie_addr;
	/* The sum of all memc views must also be a power of 2 */
	*rc_bar2_size = 1ULL << fls64(size - 1);

	/*
	 * We validate the inbound memory view even though we should trust
	 * whatever the device-tree provides. This is because of an HW issue on
	 * early Raspberry Pi 4's revisions (bcm2711). It turns out its
	 * firmware has to dynamically edit dma-ranges due to a bug on the
	 * PCIe controller integration, which prohibits any access above the
	 * lower 3GB of memory. Given this, we decided to keep the dma-ranges
	 * in check, avoiding hard to debug device-tree related issues in the
	 * future:
	 *
	 * The PCIe host controller by design must set the inbound viewport to
	 * be a contiguous arrangement of all of the system's memory.  In
	 * addition, its size mut be a power of two.  To further complicate
	 * matters, the viewport must start on a pcie-address that is aligned
	 * on a multiple of its size.  If a portion of the viewport does not
	 * represent system memory -- e.g. 3GB of memory requires a 4GB
	 * viewport -- we can map the outbound memory in or after 3GB and even
	 * though the viewport will overlap the outbound memory the controller
	 * will know to send outbound memory downstream and everything else
	 * upstream.
	 *
	 * For example:
	 *
	 * - The best-case scenario, memory up to 3GB, is to place the inbound
	 *   region in the first 4GB of pcie-space, as some legacy devices can
	 *   only address 32bits. We would also like to put the MSI under 4GB
	 *   as well, since some devices require a 32bit MSI target address.
	 *
	 * - If the system memory is 4GB or larger we cannot start the inbound
	 *   region at location 0 (since we have to allow some space for
	 *   outbound memory @ 3GB). So instead it will  start at the 1x
	 *   multiple of its size
	 */
	if (!*rc_bar2_size || (*rc_bar2_offset & (*rc_bar2_size - 1)) ||
	    (*rc_bar2_offset < SZ_4G && *rc_bar2_offset > SZ_2G)) {
		dev_err(dev, "Invalid rc_bar2_offset/size: size 0x%llx, off 0x%llx\n",
			*rc_bar2_size, *rc_bar2_offset);
		return -EINVAL;
	}

	return 0;
}

static int brcm_pcie_setup(struct brcm_pcie *pcie)
{
	struct pci_host_bridge *bridge = pci_host_bridge_from_priv(pcie);
	u64 rc_bar2_offset, rc_bar2_size;
	void __iomem *base = pcie->base;
	struct device *dev = pcie->dev;
	struct resource_entry *entry;
	bool ssc_good = false;
	struct resource *res;
	int num_out_wins = 0;
	u16 nlw, cls, lnksta;
	int i, ret, memc;
	u32 tmp, burst, aspm_support;

	/* Reset the bridge */
	pcie->bridge_sw_init_set(pcie, 1);

	/* BCM7278 fails when PERST# is set here */
	if (pcie->type != BCM7278 && pcie->type != BCM2712)
		pcie->perst_set(pcie, 1);

	usleep_range(100, 200);

	/* Take the bridge out of reset */
	pcie->bridge_sw_init_set(pcie, 0);

	tmp = readl(base + HARD_DEBUG(pcie));
	tmp &= ~PCIE_MISC_HARD_PCIE_HARD_DEBUG_SERDES_IDDQ_MASK;
	writel(tmp, base + HARD_DEBUG(pcie));
	/* Wait for SerDes to be stable */
	usleep_range(100, 200);

	/*
	 * SCB_MAX_BURST_SIZE is a two bit field.  For GENERIC chips it
	 * is encoded as 0=128, 1=256, 2=512, 3=Rsvd, for BCM7278 it
	 * is encoded as 0=Rsvd, 1=128, 2=256, 3=512.
	 */
	if (pcie->type == BCM2711)
		burst = 0x0; /* 128B */
	else if (pcie->type == BCM7278)
		burst = 0x3; /* 512 bytes */
	else
		burst = 0x2; /* 512 bytes */

	/* Set SCB_MAX_BURST_SIZE, CFG_READ_UR_MODE, SCB_ACCESS_EN */
	tmp = readl(base + PCIE_MISC_MISC_CTRL);
	u32p_replace_bits(&tmp, 1, PCIE_MISC_MISC_CTRL_SCB_ACCESS_EN_MASK);
	u32p_replace_bits(&tmp, 1, PCIE_MISC_MISC_CTRL_CFG_READ_UR_MODE_MASK);
	u32p_replace_bits(&tmp, burst, PCIE_MISC_MISC_CTRL_MAX_BURST_SIZE_MASK);
	writel(tmp, base + PCIE_MISC_MISC_CTRL);

	ret = brcm_pcie_get_rc_bar2_size_and_offset(pcie, &rc_bar2_size,
						    &rc_bar2_offset);
	if (ret)
		return ret;

	tmp = lower_32_bits(rc_bar2_offset);
	u32p_replace_bits(&tmp, brcm_pcie_encode_ibar_size(rc_bar2_size),
			  PCIE_MISC_RC_BAR2_CONFIG_LO_SIZE_MASK);
	writel(tmp, base + PCIE_MISC_RC_BAR2_CONFIG_LO);
	writel(upper_32_bits(rc_bar2_offset),
	       base + PCIE_MISC_RC_BAR2_CONFIG_HI);

	if (pcie->type == BCM2712) {
		tmp = readl(base + PCIE_MISC_UBUS_BAR2_CONFIG_REMAP);
		tmp |= PCIE_MISC_UBUS_BAR2_CONFIG_REMAP_ACCESS_EN_MASK;
		writel(tmp, base + PCIE_MISC_UBUS_BAR2_CONFIG_REMAP);
	}

	tmp = readl(base + PCIE_MISC_MISC_CTRL);
	for (memc = 0; memc < pcie->num_memc; memc++) {
		u32 scb_size_val = ilog2(pcie->memc_size[memc]) - 15;

		if (memc == 0)
			u32p_replace_bits(&tmp, scb_size_val, SCB_SIZE_MASK(0));
		else if (memc == 1)
			u32p_replace_bits(&tmp, scb_size_val, SCB_SIZE_MASK(1));
		else if (memc == 2)
			u32p_replace_bits(&tmp, scb_size_val, SCB_SIZE_MASK(2));
	}
	writel(tmp, base + PCIE_MISC_MISC_CTRL);

	/*
	 * We ideally want the MSI target address to be located in the 32bit
	 * addressable memory area. Some devices might depend on it. This is
	 * possible either when the inbound window is located above the lower
	 * 4GB or when the inbound area is smaller than 4GB (taking into
	 * account the rounding-up we're forced to perform).
	 */
	if (rc_bar2_offset >= SZ_4G || (rc_bar2_size + rc_bar2_offset) < SZ_4G)
		pcie->msi_target_addr = BRCM_MSI_TARGET_ADDR_LT_4GB;
	else
		pcie->msi_target_addr = BRCM_MSI_TARGET_ADDR_GT_4GB;

	/* disable the PCIe->GISB memory window (RC_BAR1) */
	tmp = readl(base + PCIE_MISC_RC_BAR1_CONFIG_LO);
	tmp &= ~PCIE_MISC_RC_BAR1_CONFIG_LO_SIZE_MASK;
	writel(tmp, base + PCIE_MISC_RC_BAR1_CONFIG_LO);

	/* disable the PCIe->SCB memory window (RC_BAR3) */
	tmp = readl(base + PCIE_MISC_RC_BAR3_CONFIG_LO);
	tmp &= ~PCIE_MISC_RC_BAR3_CONFIG_LO_SIZE_MASK;
	writel(tmp, base + PCIE_MISC_RC_BAR3_CONFIG_LO);

	if (pcie->gen)
		brcm_pcie_set_gen(pcie, pcie->gen);

	/* Unassert the fundamental reset */
	pcie->perst_set(pcie, 0);

	/*
	 * Give the RC/EP time to wake up, before trying to configure RC.
	 * Intermittently check status for link-up, up to a total of 100ms.
	 */
	for (i = 0; i < 100 && !brcm_pcie_link_up(pcie); i += 5)
		msleep(5);

	if (!brcm_pcie_link_up(pcie)) {
		dev_err(dev, "link down\n");
		return -ENODEV;
	}

	if (!brcm_pcie_rc_mode(pcie)) {
		dev_err(dev, "PCIe misconfigured; is in EP mode\n");
		return -EINVAL;
	}

	resource_list_for_each_entry(entry, &bridge->windows) {
		res = entry->res;

		if (resource_type(res) != IORESOURCE_MEM)
			continue;

		if (num_out_wins >= BRCM_NUM_PCIE_OUT_WINS) {
			dev_err(pcie->dev, "too many outbound wins\n");
			return -EINVAL;
		}

		brcm_pcie_set_outbound_win(pcie, num_out_wins, res->start,
					   res->start - entry->offset,
					   resource_size(res));
		num_out_wins++;
	}

	/* Don't advertise L0s capability if 'aspm-no-l0s' */
	aspm_support = PCIE_LINK_STATE_L1;
	if (!of_property_read_bool(pcie->np, "aspm-no-l0s"))
		aspm_support |= PCIE_LINK_STATE_L0S;
	tmp = readl(base + PCIE_RC_CFG_PRIV1_LINK_CAPABILITY);
	u32p_replace_bits(&tmp, aspm_support,
		PCIE_RC_CFG_PRIV1_LINK_CAPABILITY_ASPM_SUPPORT_MASK);
	writel(tmp, base + PCIE_RC_CFG_PRIV1_LINK_CAPABILITY);

	/*
	 * For config space accesses on the RC, show the right class for
	 * a PCIe-PCIe bridge (the default setting is to be EP mode).
	 */
	tmp = readl(base + PCIE_RC_CFG_PRIV1_ID_VAL3);
	u32p_replace_bits(&tmp, 0x060400,
			  PCIE_RC_CFG_PRIV1_ID_VAL3_CLASS_CODE_MASK);
	writel(tmp, base + PCIE_RC_CFG_PRIV1_ID_VAL3);

	if (pcie->ssc) {
		ret = brcm_pcie_set_ssc(pcie);
		if (ret == 0)
			ssc_good = true;
		else
			dev_err(dev, "failed attempt to enter ssc mode\n");
	}

	lnksta = readw(base + BRCM_PCIE_CAP_REGS + PCI_EXP_LNKSTA);
	cls = FIELD_GET(PCI_EXP_LNKSTA_CLS, lnksta);
	nlw = FIELD_GET(PCI_EXP_LNKSTA_NLW, lnksta);
	dev_info(dev, "link up, %s x%u %s\n",
		 pci_speed_string(pcie_link_speed[cls]), nlw,
		 ssc_good ? "(SSC)" : "(!SSC)");

	/* PCIe->SCB endian mode for BAR */
	tmp = readl(base + PCIE_RC_CFG_VENDOR_VENDOR_SPECIFIC_REG1);
	u32p_replace_bits(&tmp, PCIE_RC_CFG_VENDOR_SPCIFIC_REG1_LITTLE_ENDIAN,
		PCIE_RC_CFG_VENDOR_VENDOR_SPECIFIC_REG1_ENDIAN_MODE_BAR2_MASK);
	writel(tmp, base + PCIE_RC_CFG_VENDOR_VENDOR_SPECIFIC_REG1);

	return 0;
}

/* L23 is a low-power PCIe link state */
static void brcm_pcie_enter_l23(struct brcm_pcie *pcie)
{
	void __iomem *base = pcie->base;
	int l23, i;
	u32 tmp;

	/* Assert request for L23 */
	tmp = readl(base + PCIE_MISC_PCIE_CTRL);
	u32p_replace_bits(&tmp, 1, PCIE_MISC_PCIE_CTRL_PCIE_L23_REQUEST_MASK);
	writel(tmp, base + PCIE_MISC_PCIE_CTRL);

	/* Wait up to 36 msec for L23 */
	tmp = readl(base + PCIE_MISC_PCIE_STATUS);
	l23 = FIELD_GET(PCIE_MISC_PCIE_STATUS_PCIE_LINK_IN_L23_MASK, tmp);
	for (i = 0; i < 15 && !l23; i++) {
		usleep_range(2000, 2400);
		tmp = readl(base + PCIE_MISC_PCIE_STATUS);
		l23 = FIELD_GET(PCIE_MISC_PCIE_STATUS_PCIE_LINK_IN_L23_MASK,
				tmp);
	}

	if (!l23)
		dev_err(pcie->dev, "failed to enter low-power link state\n");
}

static int brcm_phy_cntl(struct brcm_pcie *pcie, const int start)
{
	static const u32 shifts[PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_NFLDS] = {
		PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_PWRDN_SHIFT,
		PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_RESET_SHIFT,
		PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_DIG_RESET_SHIFT,};
	static const u32 masks[PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_NFLDS] = {
		PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_PWRDN_MASK,
		PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_RESET_MASK,
		PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_DIG_RESET_MASK,};
	const int beg = start ? 0 : PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_NFLDS - 1;
	const int end = start ? PCIE_DVT_PMU_PCIE_PHY_CTRL_DAST_NFLDS : -1;
	u32 tmp, combined_mask = 0;
	u32 val;
	void __iomem *base = pcie->base;
	int i, ret;

	for (i = beg; i != end; start ? i++ : i--) {
		val = start ? BIT_MASK(shifts[i]) : 0;
		tmp = readl(base + PCIE_DVT_PMU_PCIE_PHY_CTRL);
		tmp = (tmp & ~masks[i]) | (val & masks[i]);
		writel(tmp, base + PCIE_DVT_PMU_PCIE_PHY_CTRL);
		usleep_range(50, 200);
		combined_mask |= masks[i];
	}

	tmp = readl(base + PCIE_DVT_PMU_PCIE_PHY_CTRL);
	val = start ? combined_mask : 0;

	ret = (tmp & combined_mask) == val ? 0 : -EIO;
	if (ret)
		dev_err(pcie->dev, "failed to %s phy\n", (start ? "start" : "stop"));

	return ret;
}

static inline int brcm_phy_start(struct brcm_pcie *pcie)
{
	return pcie->has_phy ? brcm_phy_cntl(pcie, 1) : 0;
}

static inline int brcm_phy_stop(struct brcm_pcie *pcie)
{
	return pcie->has_phy ? brcm_phy_cntl(pcie, 0) : 0;
}

static void brcm_pcie_turn_off(struct brcm_pcie *pcie)
{
	void __iomem *base = pcie->base;
	int tmp;

	if (brcm_pcie_link_up(pcie))
		brcm_pcie_enter_l23(pcie);
	/* Assert fundamental reset */
	pcie->perst_set(pcie, 1);

	/* Deassert request for L23 in case it was asserted */
	tmp = readl(base + PCIE_MISC_PCIE_CTRL);
	u32p_replace_bits(&tmp, 0, PCIE_MISC_PCIE_CTRL_PCIE_L23_REQUEST_MASK);
	writel(tmp, base + PCIE_MISC_PCIE_CTRL);

	/* Turn off SerDes */
	tmp = readl(base + HARD_DEBUG(pcie));
	u32p_replace_bits(&tmp, 1, PCIE_MISC_HARD_PCIE_HARD_DEBUG_SERDES_IDDQ_MASK);
	writel(tmp, base + HARD_DEBUG(pcie));

	/* Shutdown PCIe bridge */
	pcie->bridge_sw_init_set(pcie, 1);
}

static int pci_dev_may_wakeup(struct pci_dev *dev, void *data)
{
	bool *ret = data;

	if (device_may_wakeup(&dev->dev)) {
		*ret = true;
		dev_info(&dev->dev, "disable cancelled for wake-up device\n");
	}
	return (int) *ret;
}

static void set_regulators(struct brcm_pcie *pcie, bool on)
{
	struct device *dev = pcie->dev;
	struct pci_host_bridge *bridge = pci_host_bridge_from_priv(pcie);
	struct list_head *pos;

	if (list_empty(&pcie->pwr_supplies))
		return;

	if (on) {
		if (pcie->ep_wakeup_capable) {
			/*
			 * We are resuming from a suspend.  In the suspend we
			 * did not disable the power supplies, so there is
			 * no need to enable them (and falsely increase their
			 * usage count).
			 */
			pcie->ep_wakeup_capable = false;
			return;
		}
	} else {
		/*
		 * If at least one device on this bus is enabled as a wake-up
		 * source, do not turn off regulators
		 */
		pcie->ep_wakeup_capable = false;
		if (bridge->bus && brcm_pcie_link_up(pcie)) {
			pci_walk_bus(bridge->bus, pci_dev_may_wakeup, &pcie->ep_wakeup_capable);
			if (pcie->ep_wakeup_capable)
				return;
		}
	}

	list_for_each(pos, &pcie->pwr_supplies) {
		struct brcm_dev_pwr_supply *supply
			= list_entry(pos, struct brcm_dev_pwr_supply, node);
		int ret;

		if (on) {
			ret = regulator_enable(supply->regulator);
			dev_info(dev, "%senable %s regulator.\n",
				 ret ? "failed to " : "", supply->name);
		} else {
			ret = regulator_disable(supply->regulator);
			dev_info(dev, "%sdisable %s regulator.\n",
				 ret ? "failed to " : "", supply->name);
		}
	}
}

static void config_clkreq(struct brcm_pcie *pcie)
{
	void __iomem *base = pcie->base;
	struct pci_host_bridge *bridge = pci_host_bridge_from_priv(pcie);
	int domain = pci_domain_nr(bridge->bus);
	const struct pci_bus *bus = pci_find_bus(domain, 1);
	struct pci_dev *pdev;
	u32 tmp, link_cap = 0;
	u16 link_ctl = 0;

	pdev = bus ? (struct pci_dev *)bus->devices.next : NULL;
	if (!pdev)
		return;

	pcie_capability_read_dword(pdev, PCI_EXP_LNKCAP, &link_cap);
	if (!(link_cap & PCI_EXP_LNKCAP_CLKPM))
		return;

	pcie_capability_read_word(pdev, PCI_EXP_LNKCTL, &link_ctl);
	if (!(link_ctl & PCI_EXP_LNKCTL_CLKREQ_EN))
		return;
	/*
	 * Refclk from RC should be gated with CLKREQ# input when ASPM L0s,L1
	 * is enabled => setting the CLKREQ_DEBUG_ENABLE field to 1.
	 */
	tmp = readl(base + HARD_DEBUG(pcie));
	tmp |= PCIE_MISC_HARD_PCIE_HARD_DEBUG_CLKREQ_DEBUG_ENABLE_MASK;
	writel(tmp, base + HARD_DEBUG(pcie));
	dev_info(pcie->dev, "clkreq control enabled\n");
}

static int brcm_pcie_suspend(struct device *dev)
{
	struct brcm_pcie *pcie = dev_get_drvdata(dev);
	int ret;

	set_regulators(pcie, false);
	brcm_pcie_turn_off(pcie);
	ret = brcm_phy_stop(pcie);
	if (ret)
		dev_err(pcie->dev, "failed to stop phy\n");
	reset_control_rearm(pcie->rescal);
	clk_disable_unprepare(pcie->clk);

	return ret;
}

static int brcm_pcie_resume(struct device *dev)
{
	struct brcm_pcie *pcie = dev_get_drvdata(dev);
	void __iomem *base;
	u32 tmp;
	int ret;

	base = pcie->base;
	ret = clk_prepare_enable(pcie->clk);
	if (ret)
		return ret;

	/* Take the bridge out of reset so we can access Rescal and SERDES */
	pcie->bridge_sw_init_set(pcie, 0);

	ret = reset_control_reset(pcie->rescal);
	if (ret)
		goto err_disable_clk;
	set_regulators(pcie, true);

	ret = reset_control_reset(pcie->rescal);
	if (ret)
		goto err_disable_clk;

	ret = brcm_phy_start(pcie);
	if (ret) {
		dev_err(pcie->dev, "failed to start phy\n");
		goto err_reset;
	}

	/* SERDES_IDDQ = 0 */
	tmp = readl(base + HARD_DEBUG(pcie));
	u32p_replace_bits(&tmp, 0, PCIE_MISC_HARD_PCIE_HARD_DEBUG_SERDES_IDDQ_MASK);
	writel(tmp, base + HARD_DEBUG(pcie));

	/* wait for serdes to be stable */
	udelay(100);

	ret = brcm_pcie_setup(pcie);
	if (ret)
		goto err_reset;

	if (pcie->msi)
		brcm_msi_set_regs(pcie->msi);
	config_clkreq(pcie);

	return 0;

err_reset:
	reset_control_rearm(pcie->rescal);
err_disable_clk:
	clk_disable_unprepare(pcie->clk);
	return ret;
}

static void __brcm_pcie_remove(struct brcm_pcie *pcie)
{
	brcm_msi_remove(pcie);
	set_regulators(pcie, false);
	brcm_pcie_turn_off(pcie);
	if (brcm_phy_stop(pcie))
		dev_err(pcie->dev, "failed to stop phy\n");
	reset_control_rearm(pcie->rescal);
	clk_disable_unprepare(pcie->clk);
}

static int brcm_pcie_remove(struct platform_device *pdev)
{
	struct brcm_pcie *pcie = platform_get_drvdata(pdev);
	struct pci_host_bridge *bridge = pci_host_bridge_from_priv(pcie);

	pci_stop_root_bus(bridge->bus);
	if (pcie->has_err_report)
		brcm_unregister_die_notifiers(pcie);
	pci_remove_root_bus(bridge->bus);
	__brcm_pcie_remove(pcie);

	return 0;
}

static void brcm_pcie_shutdown(struct platform_device *pdev)
{
	struct brcm_pcie *pcie = platform_get_drvdata(pdev);

	if (pcie->has_err_report)
		brcm_unregister_die_notifiers(pcie);
	__brcm_pcie_remove(pcie);
}

static const struct of_device_id brcm_pcie_match[] = {
	{ .compatible = "brcm,bcm2711-pcie", .data = &bcm2711_cfg },
	{ .compatible = "brcm,bcm7211-pcie", .data = &generic_cfg },
	{ .compatible = "brcm,bcm7278-pcie", .data = &bcm7278_cfg },
	{ .compatible = "brcm,bcm7216-pcie", .data = &bcm7216_cfg },
	{ .compatible = "brcm,bcm7445-pcie", .data = &generic_cfg },
	{ .compatible = "brcm,bcm2712-pcie", .data = &bcm2712_cfg },
	{ .compatible = "brcm,bcm7712-pcie", .data = &bcm2712_cfg },
	{ .compatible = "brcm,bcm7212-pcie", .data = &bcm2712_cfg },
	{},
};

static int brcm_pcie_probe(struct platform_device *pdev)
{
	struct device_node *np = pdev->dev.of_node, *msi_np;
	struct pci_host_bridge *bridge;
	const struct pcie_cfg_data *data;
	struct brcm_pcie *pcie;
	struct brcm_dev_pwr_supply *supply;
	const char *name;
	int supplies;
	int ret, i;

	bridge = devm_pci_alloc_host_bridge(&pdev->dev, sizeof(*pcie));
	if (!bridge)
		return -ENOMEM;

	data = of_device_get_match_data(&pdev->dev);
	if (!data) {
		pr_err("failed to look up compatible string\n");
		return -EINVAL;
	}

	pcie = pci_host_bridge_priv(bridge);
	pcie->dev = &pdev->dev;
	pcie->np = np;
	pcie->reg_offsets = data->offsets;
	pcie->type = data->type;
	pcie->has_err_report = data->has_err_report;
	pcie->perst_set = data->perst_set;
	pcie->bridge_sw_init_set = data->bridge_sw_init_set;
	pcie->has_phy = data->has_phy;

	pcie->base = devm_platform_ioremap_resource(pdev, 0);
	if (IS_ERR(pcie->base))
		return PTR_ERR(pcie->base);

	pcie->clk = devm_clk_get_optional(&pdev->dev, "sw_pcie");
	if (IS_ERR(pcie->clk))
		return PTR_ERR(pcie->clk);

	ret = of_pci_get_max_link_speed(np);
	pcie->gen = (ret < 0) ? 0 : ret;

	pcie->ssc = of_property_read_bool(np, "brcm,enable-ssc");

	ret = clk_prepare_enable(pcie->clk);
	if (ret) {
		dev_err(&pdev->dev, "could not enable clock\n");
		return ret;
	}

	pcie->swinit = devm_reset_control_get_optional_exclusive(&pdev->dev, "swinit");
	if (IS_ERR(pcie->swinit)) {
		ret = PTR_ERR(pcie->swinit);
		goto clk_out;
	}

	pcie->bridge = devm_reset_control_get_optional_exclusive(&pdev->dev, "bridge");
	if (IS_ERR(pcie->bridge)) {
		ret = PTR_ERR(pcie->bridge);
		goto clk_out;
	}

	/* Some chips require us to do this before accessing any registers */
	pcie->bridge_sw_init_set(pcie, 0);

	ret = reset_control_assert(pcie->swinit);
	if (ret) {
		goto bridge_reset_out;
	} else {
		ret = reset_control_deassert(pcie->swinit);
		if (ret)
			goto bridge_reset_out;
	}

	pcie->rescal = devm_reset_control_get_optional_shared(&pdev->dev, "rescal");
	if (IS_ERR(pcie->rescal)) {
		ret = PTR_ERR(pcie->rescal);
		goto bridge_reset_out;
	}

	ret = reset_control_reset(pcie->rescal);
	if (ret)
		goto bridge_reset_out;

	ret = brcm_phy_start(pcie);
	if (ret) {
		dev_err(pcie->dev, "failed to start phy\n");
		goto rescal_reset_out;
	}

	INIT_LIST_HEAD(&pcie->pwr_supplies);
	supplies = of_property_count_strings(np, "supply-names");
	if (supplies <= 0)
		supplies = 0;

	for (i = 0; i < supplies; i++) {
		if (of_property_read_string_index(np, "supply-names", i,
						  &name))
			continue;
		supply = devm_kzalloc(&pdev->dev, sizeof(*supply), GFP_KERNEL);
		if (!supply)
			return -ENOMEM;
		strncpy(supply->name, name, sizeof(supply->name));
		supply->name[sizeof(supply->name) - 1] = '\0';
		supply->regulator = devm_regulator_get(&pdev->dev, name);
		if (IS_ERR(supply->regulator)) {
			dev_err(&pdev->dev, "Unable to get %s supply, err=%d\n",
				name, (int)PTR_ERR(supply->regulator));
			continue;
		}
		list_add_tail(&supply->node, &pcie->pwr_supplies);
	}
	set_regulators(pcie, true);

	ret = brcm_pcie_setup(pcie);
	if (ret)
		goto fail;

	pcie->hw_rev = readl(pcie->base + PCIE_MISC_REVISION);

	msi_np = of_parse_phandle(pcie->np, "msi-parent", 0);
	if (pci_msi_enabled() && msi_np == pcie->np) {
		ret = brcm_pcie_enable_msi(pcie);
		if (ret) {
			dev_err(pcie->dev, "probe of internal MSI failed");
			goto fail;
		}
	}

	bridge->ops = &brcm_pcie_ops;
	bridge->sysdata = pcie;

	platform_set_drvdata(pdev, pcie);

	if (pcie->has_err_report)
		brcm_register_die_notifiers(pcie);

	ret = pci_host_probe(bridge);
	if (!ret)
		config_clkreq(pcie);
	return ret;
fail:
	__brcm_pcie_remove(pcie);
	return ret;

rescal_reset_out:
	reset_control_rearm(pcie->rescal);
bridge_reset_out:
	pcie->bridge_sw_init_set(pcie, 1);
clk_out:
	clk_disable_unprepare(pcie->clk);
	return ret;
}

MODULE_DEVICE_TABLE(of, brcm_pcie_match);

static const struct dev_pm_ops brcm_pcie_pm_ops = {
	.suspend_noirq = brcm_pcie_suspend,
	.resume_noirq = brcm_pcie_resume,
};

static struct platform_driver brcm_pcie_driver = {
	.probe = brcm_pcie_probe,
	.remove = brcm_pcie_remove,
	.shutdown = brcm_pcie_shutdown,
	.driver = {
		.name = "brcm-pcie",
		.of_match_table = brcm_pcie_match,
		.pm = &brcm_pcie_pm_ops,
	},
};
module_platform_driver(brcm_pcie_driver);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("Broadcom STB PCIe RC driver");
MODULE_AUTHOR("Broadcom");
