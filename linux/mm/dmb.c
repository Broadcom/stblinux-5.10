// SPDX-License-Identifier: GPL-2.0+
/*
 * Designated Movable Block
 */

#define pr_fmt(fmt) "dmb: " fmt

#include <linux/dmb.h>

struct dmb {
	unsigned long start_pfn;
	unsigned long end_pfn;
};

static struct dmb dmb_areas[CONFIG_DMB_COUNT];
static unsigned int dmb_area_count;

int __init dmb_reserve(phys_addr_t base, phys_addr_t size)
{
	phys_addr_t alignment;
	int ret;

	if (!size)
		return -EINVAL;

	/* ensure minimal alignment required by mm core */
	alignment = PAGE_SIZE <<
			max_t(unsigned long, MAX_ORDER - 1, pageblock_order);

	if (ALIGN(base, alignment) != base || ALIGN(size, alignment) != size)
		return -EINVAL;

	if (!memblock_is_region_memory(base, size) ||
	    memblock_is_region_reserved(base, size))
		return -ENOMEM;

	ret = memblock_reserve(base, size);
	if (ret)
		return ret;

	memblock_mark_movable(base, size);
	return 0;
}

unsigned long __init dmb_declare(unsigned long start, unsigned long end)
{
	struct dmb *dmb;
	unsigned long alignment, pages;

	alignment = 1UL << max_t(unsigned long, MAX_ORDER - 1, pageblock_order);
	start = ALIGN(start, alignment);
	end = ALIGN_DOWN(end, alignment);
	if (end <= start)
		return 0;

	if (dmb_area_count == ARRAY_SIZE(dmb_areas)) {
		pr_warn("Not enough slots for DMB reserved region 0x%lx000 - 0x%lx000!\n",
			start, end);
		return 0;
	}

	pages = end - start;

	dmb = &dmb_areas[dmb_area_count++];
	dmb->start_pfn = start;
	dmb->end_pfn = end;

	return pages;
}

int dmb_intersects(unsigned long spfn, unsigned long epfn)
{
	int i;
	struct dmb *dmb;

	if (spfn >= epfn)
		return DMB_DISJOINT;

	for (i = 0; i < dmb_area_count; i++) {
		dmb = &dmb_areas[i];
		if (spfn >= dmb->end_pfn)
			continue;
		if (epfn <= dmb->start_pfn)
			return DMB_DISJOINT;
		if (spfn >= dmb->start_pfn && epfn <= dmb->end_pfn)
			return DMB_INTERSECTS;
		else
			return DMB_MIXED;
	}

	return DMB_DISJOINT;
}

void __init dmb_init_region(struct memblock_region *region)
{
	unsigned long pfn;
	int i;

	for (pfn = memblock_region_memory_base_pfn(region);
	     pfn < memblock_region_memory_end_pfn(region);
	     pfn += pageblock_nr_pages) {
		struct page *page = pfn_to_page(pfn);

		for (i = 0; i < pageblock_nr_pages; i++)
			set_page_zone(page + i, ZONE_MOVABLE);

		/* free reserved pageblocks to page allocator */
		init_reserved_pageblock(page);
	}
}

/*
 * Support for reserved memory regions defined in device tree
 */
#ifdef CONFIG_OF_RESERVED_MEM
#include <linux/of.h>
#include <linux/of_fdt.h>
#include <linux/of_reserved_mem.h>

#undef pr_fmt
#define pr_fmt(fmt) fmt

static int __init rmem_dmb_setup(struct reserved_mem *rmem)
{
	phys_addr_t align = PAGE_SIZE << max(MAX_ORDER - 1, pageblock_order);
	phys_addr_t mask = align - 1;
	unsigned long node = rmem->fdt_node;

	if (!of_get_flat_dt_prop(node, "reusable", NULL) ||
	    of_get_flat_dt_prop(node, "no-map", NULL))
		return -EINVAL;

	if ((rmem->base & mask) || (rmem->size & mask)) {
		pr_err("Reserved memory: incorrect alignment of DMB region\n");
		return -EINVAL;
	}

	memblock_mark_movable(rmem->base, rmem->size);

	pr_info("Reserved memory: created DMB at %pa, size %ld MiB\n",
		&rmem->base, (unsigned long)rmem->size / SZ_1M);

	return 0;
}
RESERVEDMEM_OF_DECLARE(dmb, "designated-movable-block", rmem_dmb_setup);
#endif
