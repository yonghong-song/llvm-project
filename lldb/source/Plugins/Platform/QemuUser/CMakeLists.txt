lldb_tablegen(PlatformQemuUserProperties.inc -gen-lldb-property-defs
  SOURCE PlatformQemuUserProperties.td
  TARGET LLDBPluginPlatformQemuUserPropertiesGen)

lldb_tablegen(PlatformQemuUserPropertiesEnum.inc -gen-lldb-property-enum-defs
  SOURCE PlatformQemuUserProperties.td
  TARGET LLDBPluginPlatformQemuUserPropertiesEnumGen)

add_lldb_library(lldbPluginPlatformQemuUser PLUGIN
  PlatformQemuUser.cpp

  LINK_COMPONENTS
    Support
  LINK_LIBS
    lldbUtility
    )

add_dependencies(lldbPluginPlatformQemuUser
  LLDBPluginPlatformQemuUserPropertiesGen
  LLDBPluginPlatformQemuUserPropertiesEnumGen)
