#How to write Controllers.

---

# How To #
  * For an URL to map to a handler, you should state it in the configuration file in the following way:

```
        uri {
          path = "url-to-the-page";
          service {
            type = "dynamic";
            handler = "name-of-the-controller";
          }
        };
```

  * Register the handler in the handler list in controllers.ml (the variable with the name
> > handlers)
> > The file `server_utils.ml` provides a convenient way to define handlers that return a view in an easy way, calling the function `template_process`. It accepts one required parameter, `view`, which must be the name you registered as the handler in the configuration file, and some optional parameters:

  * `bind_parameters`: Should be a method that accepts a cgi request and return a list of tuples (string, expression) to use in the template view. The first element of the tuple is the variable name used by the template, and the second elements is of type `expression`, which is defined in file `expression_evaluator.ml` as follows:

```
type expression =
  | EXP  of slp_basic_type
  | LIST of expression list
  | DICT of (string * expression) list
  | NULL
;;
```

where slp\_basic\_type is defined as:

```
type slp_numeric =
  | Int of int
  | Float of float
;;

type slp_basic_type =
  | Numeric of slp_numeric
  | Boolean of bool
  | String of string
  | Char of char
;;
```

  * `headers`: Accepts a list of tuples (string, string) which are names and values for optional headers.

  * `template_fie_function`: Function that accepts the view as parameter (it can ignore it) and returns a template file. By default, it returns the file _view_.fhtml in the `template_dir` directory.

  * `path`: Rceives an string indicating the actual directory in case the template file is located other site than the `template_dir` directory.

Example of a simple _hello world_:

```
  let hello_function cgi =
    match get_variable_or_default cgi "name" ~default:(Some "word") with
      | None -> assert false (* Should never happen *)
      | Some name -> ["name", EXP (Basic (String name))]
  ;;

  let handlers = [
    template_process "hello"
      ~bind_parameters:hello_function
      ~template_file_function:(function _ -> "template.html")
      ~headers:["Content-Type", "text/plain; charset=\"iso-8859-1\""]
  ;;
```

and the template file, in this case "template.html":
```
    <html><head><title>Hello world</title></head>
      <body>
        Hello, {{ name }}
      </body>
    </html>
```