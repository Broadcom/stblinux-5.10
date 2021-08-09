################################################################################
#
# brcm-skel
#
################################################################################

BRCMROOT_VERSION = $(call qstrip,$(BR2_BRCM_STB_TOOLS_VERSION))

BRCM_SKEL_VERSION = $(BRCMROOT_VERSION)
BRCM_SKEL_SITE = git://stbgit.stb.broadcom.net/queue/stbtools.git
BRCM_SKEL_SOURCE = stbtools-$(BRCMROOT_VERSION).tar.gz
BRCM_SKEL_DL_SUBDIR = brcm-pm
BRCM_SKEL_LICENSE = GPL-2.0

BRCM_SKEL_FSTAB_INIT = $(TARGET_DIR)/etc/fstab.init
BRCM_SKEL_FSTAB_SYSTEMD = $(TARGET_DIR)/etc/fstab.systemd

# Select the fstab we want, and the one we don't want, based on the init system.
# If we just have a single, generic fstab, the code below won't do anything.
ifeq ($(BR2_INIT_SYSTEMD),y)
BRCM_SKEL_FSTAB_SRC = $(BRCM_SKEL_FSTAB_SYSTEMD)
BRCM_SKEL_FSTAB_ALT = $(BRCM_SKEL_FSTAB_INIT)
else
BRCM_SKEL_FSTAB_SRC = $(BRCM_SKEL_FSTAB_INIT)
BRCM_SKEL_FSTAB_ALT = $(BRCM_SKEL_FSTAB_SYSTEMD)
endif

ifeq ($(BR2_INIT_BUSYBOX),y)
# Ensure that packages initscripts & busybox don't overwrite any of our files
BRCM_SKEL_DEPENDENCIES = initscripts busybox
else
# Don't install rcS if we have neither INIT_BUSYBOX nor INIT_SYSV
ifeq ($(BR2_INIT_SYSV),)
BRCM_SKEL_INSTALL_EXCLUDE = --exclude init.d
endif
endif

# Extract only what we need to save space.
define BRCM_SKEL_EXTRACT_CMDS
	$(call suitable-extractor,$(BRCM_SKEL_SOURCE)) \
		$(BRCM_SKEL_DL_DIR)/$(BRCM_SKEL_SOURCE) | \
		$(TAR) --strip-components=1 -C $(BRCM_SKEL_DIR) \
			--wildcards $(TAR_OPTIONS) - '*/skel'
endef

# If we have separate fstab files per init system, install the matching one.
ifneq (,$(wildcard $(BRCM_SKEL_FSTAB_SRC)))
define BRCM_SKEL_FSTAB_RENAME
	@echo "Installing `basename $(BRCM_SKEL_FSTAB_SRC)` as /etc/fstab..."
	mv "$(BRCM_SKEL_FSTAB_SRC)" "$(TARGET_DIR)/etc/fstab"
	rm -f "$(BRCM_SKEL_FSTAB_ALT)"
endef

BRCM_SKEL_POST_INSTALL_TARGET_HOOKS += BRCM_SKEL_FSTAB_RENAME
endif

define BRCM_SKEL_INSTALL_TARGET_CMDS
	rsync -a --exclude .gitignore \
		$(BRCM_SKEL_INSTALL_EXCLUDE) \
		$(@D)/skel/ $(TARGET_DIR)
endef

$(eval $(generic-package))
