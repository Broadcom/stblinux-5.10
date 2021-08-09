################################################################################
#
# brcm-ams-tracing
#
################################################################################
BRCM_AMS_KERNEL_DIR = $(LINUX_DIR)/tools/testing/brcmstb/dvfs-api/tracing

# We only need the kernel to be extracted, not actually built
BRCM_AMS_TRACING_PATCH_DEPENDENCIES = linux

define BRCM_AMS_TRACING_BUILD_CMDS
	$(Q)if test ! -f $(BRCM_AMS_KERNEL_DIR)/Makefile ; then \
		echo "Your kernel does not have the AMS tracing tools." ; \
		echo "Disable BR2_PACKAGE_BRCM_AMS_TRACING config using 'make menuconfig'" ; \
		exit 1 ; \
	fi

	$(TARGET_MAKE_ENV) \
		BUILD=$(KERNEL_ARCH) \
		$(MAKE) -C $(BRCM_AMS_KERNEL_DIR) $(TARGET_CONFIGURE_OPTS)
endef

define BRCM_AMS_TRACING_INSTALL_TARGET_CMDS
	$(INSTALL) -D -m 0755 $(BRCM_AMS_KERNEL_DIR)/trace \
		$(TARGET_DIR)/usr/bin/brcm_ams_trace
endef

$(eval $(generic-package))
