# Comparison between OCaml web frameworks #

---

|Functionality|WDialog|Ex-Nunc|Ocsigen|mod\_caml|
|:------------|:------|:------|:------|:--------|
|Continuations based framework|No|No|Yes|No|
|Static validation of xhtml|No|No. Run time validation planned|Yes|No|
|i18n support|Basic support in CVS version|No|(use a i18n library such as ocaml-gettext)|Use an external library|
|Database backend|No|Using external library|(Using external library) Advanced support under development (2007-2008)|(Using external library)|
|Session through hidden form fields (POST)|Yes|Yes|Yes|Yes|
|Session through URL parameters (GET)|Yes (through Cgi interface)|Yes|Yes|Yes|
|Session saving|Memory, external daemon, client hidden fields, database|Memory, client hidden fields|Using closures in memory, and/or database on hard disk. Cookie at client side|Cookie and database|
|Validation of typed script parameters|No|Yes|Yes|No|
|Script form|XML description + OCaml code|HTML that contains OCaml code in special tags|OCaml code to describe the logic of the site. Ocaml functions generate pages (no specific template system).|HTML templates, separate from OCaml code|
|Script interaction|OCaml functions called with simulated events|??|OCaml function calls|OCaml function calls|
|AJAX support|No|Not yet|Under development (2007-2008)|No|
|Protection against HTML injection|Yes|Yes|Yes|Yes, in templates|
|Protection against SQL injection|No|No|Yes, if using for example PG’OCaml or ocamldbi for database access|Yes, if using PG’OCaml or ocamldbi for database access|
|Development status|2.1.2|??|version 1.0|1.4.0 (CVS version)|
|Community|very small|none|small|??|

Taken from [DEMEXP](https://demexp.org/dokuwiki/doku.php?id=en:web_client_development_framework#comparison_of_ocaml_frameworks)

~~TODO~~
Link to each framework page.