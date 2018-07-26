BUILD_DIR=build
DIST_DIR=.
ECHO=echo
CMAKE=cmake
RM=rm
MKDIR_P = mkdir -p
CD=cd

default: all

checkdir:
ifeq "$(wildcard $(BUILD_DIR) )" ""
	@$(ECHO) "Build Directory not existing, creating..."
	@${MKDIR_P} ${BUILD_DIR}
endif

cmakestep: checkdir
	$(CD) $(BUILD_DIR) && $(CMAKE) ../${DIST_DIR}

all: cmakestep
	$(MAKE) -j8 -C $(BUILD_DIR) all

install: all
	$(MAKE) -C $(BUILD_DIR) install

clean:
	$(RM) -rf $(BUILD_DIR)/*

.PHONY: clean install
