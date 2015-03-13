# To-Do List #

---

  * ~~**Comment Removing**~~
  * ~~**1st Level Lexer**~~
  * ~~**2nd Level Lexer**~~
  * ~~**Expression Evaluator**~~: Expressions in the template language are evaluated to produce a response used along with static text. Expressions may include literals and read-only variables (the variables should be found in an association list)
  * ~~**Arithmetic Expressions**~~: The arithmetic expressions in the template language should include the following operators with a well defined precedence and associativity:
    * Sum
    * Substraction
    * Multiplication
    * Division
    * Module
  * ~~**Relational Expressions**~~: Relational expressions in the template language should include the following operators with a well defined precedence and associativity (it should work with strings and numbers):
    * <
    * <=
    * >
    * >=
    * =
  * ~~**Logic Expressions**~~: Logic expressions in the template language should include the following operators with a well defined precedence and associativity:
    * And
    * Or
    * Xor
    * Not
  * ~~**Expressions with Strings**~~: The expressions with strings in the template language should include the following operators or functions with a well defined precedence and associativity:
    * Length
    * Concatenation
    * Access
  * ~~**Expressions with Lists and Dictionaries**~~: The expressions with lists and dictionaries in the template language allow you to obtain an element from them, it should have an homogeneous notation

  * ~~**Web Server**~~: The Web server receives Get requests asking for static content (html, txt, css, js, jpeg, png and gif). If it doesn't find the required resource, it returns a 404 page(Not Found).

  * ~~**Configuration File**~~: The configurable details of the web server (port number, main directory) are read from a configuration file. This file can be plain text, XML, YAML, etc.

  * ~~**Post Method**~~: The Web server receives requests through POST requests. If a request, which is not GET or POST type, arrives, the server returns a 405 page(Method not allowed) which includes the Allow header, and the body contains an error message with a description.

  * ~~**Controller**~~: The web server can receive special requests that identify a controller. A controller is a program in the host language (OCaml) that gives then the execution flow to a view. The controller sends to the template an association list with the variables-values wanted.

  * ~~**Parameters**~~: The controllers receive an association list with request parameters, no matter if they are GET or POST

  * ~~**Headers**~~: The controllers receive an association list with request headers, and the controller can indicate the headers of the response.

  * ~~**Conditionals**~~: The template language includes a mechanism to include or not certain elements of a page based on a conditions.

  * ~~**Cycles**~~: The template language has a mechanism to easily iterate over all elements in a list or a dictionary.