# Erlang Xslt (Что это)

Простой многопоточный xslt-преобразователь

# Requirements (Что нужно)

* libxml2
* libxslt
* g++
* gnumake
* erlang
* rebar

# Example (Пример)

    Xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<catalog>"
    "<cd>"
        "<title>Empire Burlesque</title>"
        "<artist>Bob Dylan</artist>"
        "<country>USA</country>"
        "<company>Columbia</company>"
        "<price>10.90</price>"
        "<year>1985</year>"
        "</cd>"
    "</catalog>",

    xslt:start_link(),
    xslt:apply("priv/xsl/template.xsl", Xml).

# Credis (Кто это натворил)

* Сергей Кожевников (Serge Kozhevnikov aka cff, 2011);
* Илья w-495 Никитин (w-495, 2012).
