/* SPDX-License-Identifier: GPL-2.0 */
#ifndef __DMB_H__
#define __DMB_H__

#include <linux/memblock.h>

enum {
	DMB_DISJOINT = 0,
	DMB_INTERSECTS,
	DMB_MIXED,
};

extern int dmb_intersects(unsigned long spfn, unsigned long epfn);
extern int dmb_reserve(phys_addr_t base, phys_addr_t size);
extern unsigned long dmb_declare(unsigned long start, unsigned long end);
extern void dmb_init_region(struct memblock_region *region);

#endif
