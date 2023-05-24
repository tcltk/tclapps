tkchat::Hook add join {apply { args {
    rename sgml::PCDATA {}
    rename sgml::PCDATA_NEW sgml::PCDATA
}}}

namespace eval sgml {}
proc sgml::PCDATA_NEW {opts pcdata} {
    array set options $opts

    if {$options(-ignorewhitespace) &&
        ![string length [string trim $pcdata]]} {
        return {}
    }

    if {0 && ![regexp ^[cl $::sgml::Char]*\$ $pcdata]} {
        upvar #0 $options(-statevariable) state
        uplevel #0 $options(-errorcommand) [list illegalcharacters "illegal, non-Unicode characters found in text \"$pcdata\" around line $state(line)"]
    }

    uplevel #0 $options(-characterdatacommand) [list $pcdata]
}
