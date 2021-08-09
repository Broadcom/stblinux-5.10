// SPDX-License-Identifier: GPL-2.0-only
#include <linux/of.h>
#include <linux/kernel.h>

static int __init brcmstb_fixup_scmi(void)
{
	struct device_node *dn;
	struct property *prop;

	dn = of_find_compatible_node(NULL, NULL, "arm,scmi");
	if (!dn)
		return 0;

	prop = of_find_property(dn, "arm,smc-id", NULL);
	if (!prop)
		goto out;

	prop = of_find_property(dn, "mboxes", NULL);
	if (!prop)
		goto out;

	of_remove_property(dn, prop);
out:
	of_node_put(dn);
	return 0;
}

static int __init brcmstb_fdt_fixups(void)
{
	return brcmstb_fixup_scmi();
}
arch_initcall(brcmstb_fdt_fixups);
