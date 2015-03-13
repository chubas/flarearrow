#Description of the template language grammar

# Grammar #

Template language grammar:

template:
> `(raw_text | comment | expression)*`

raw\_text:
> _any character sequence except "{{", "{%" or "{#" unless escaped or quoted_

comment:
> _{# any character sequence except "#}" unless escaped or quoted #}_

expression:
> `single_expression | if_expression | for_expression`

for\_expression:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**{% for** `for_variables` **in** `expression_atom` **%}**
> > `template`

> <a href='Hidden comment: Wiki syntax is horrible'></a>**{% endfor %}**

for\_variables:
> `variable | `
> `variable, variable`

if\_expression:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**{% if** `expression_atom` **%}**
> > `template`

> <a href='Hidden comment: Wiki syntax is horrible'></a>**{% endfor %}**

single\_expression:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**{{** `multiple_expression_atoms` **}}**

multiple\_expression\_atoms:
> `expression_atom |`
> `expression_atom`**;;** `multiple_expression_atoms`

expression\_atom:
> `simple_expression | `
> `operator_expression`

simple\_expression:
> `parenthesized_expression |`
> `function | `
> `elemental_type`

parenthesized\_expression:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**(** `expression_atom` **)**

function:
> `identifier`**(**`list_contents`**)** |
> `simple_expression`**.[**`expression_atom`**]** |
> `simple_expression`**.{**`expression_atom`**}**

operator\_expression:
> `unary_operator expression_atom | `
> `expression_atom binary_operator expression_atom`

unary\_operator:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**not** `|` **~**

binary\_operator:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**and** `|` **or** `|` **xor** `|`
> <a href='Hidden comment: Wiki syntax is horrible'></a>**==** `|` **!=** `|` **>** `|` **<** `|` **>=** `|` **<=** `|`
> <a href='Hidden comment: Wiki syntax is horrible'></a>**+** `|` **-** `|` **`*`** `|` **/** `|` **%** `|` **^**

elemental\_type:
> `list | `
> `dict | `
> `basic_type | `
> `variable`

list:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**[** `list_contents` **]**

list\_contents:
> `expression_atom | `
> `expression_atom`**;** `list_contents `

dict\_contents:
> `dict_tuple | `
> `dict_tuple`**;** `dict_contents`

dict\_tuple:
> `string` **:** `expression_atom`

basic\_type:
> `numeric | boolean | string | char`

numeric:
> `int | float`

int:
> `digit+`

float:
> `digit+`**.**`digit+`

string:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**"**`string_contents`**"**

string\_contents:
> _any characted except \ or " unless escaped_

character:
> <a href='Hidden comment: Wiki syntax is horrible'></a>**'**_any character_**'**

variable:
> `char_id( (char_id|digit)* )*`

char\_id:
> _['A'..'Z' 'a'..'z' '`_`']_

digit:
> _'0'..'9'_

---

Escaped character sequence means putting a backslash before, e.g. putting a \#} inside a comment will not close it.

Quoted character sequence means putting it into a string, e.g. putting the string "}}" inside an expression will not terminate it.