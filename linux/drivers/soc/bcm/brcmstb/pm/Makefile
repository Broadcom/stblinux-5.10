# SPDX-License-Identifier: GPL-2.0-only
obj-y				+= regsave.o
obj-$(CONFIG_ARM_PSCI_FW)	+= pm-psci.o pm-common.o
ifndef CONFIG_ARM_PSCI_FW
obj-$(CONFIG_ARM)		+= s2-arm.o pm-arm.o
AFLAGS_s2-arm.o			:= -march=armv7-a
endif
obj-$(CONFIG_BMIPS_GENERIC)	+= s2-mips.o s3-mips.o pm-mips.o
