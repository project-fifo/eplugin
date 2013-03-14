eplugin
========
This library intends to provide a simple way to manage plugins for an erlang application, right now the abstraction is very simple and high level but it is quite flexible.

usage
-----

Simply start the applicaiton, there is only one configuration variable at the moment named `plugin_dir` which as the name suggets specifies where the plugins are discovered. It defaults to `plugsin`.

From this point on only trhee functions are really of interest: `apply/2`, `callbacks/1`, `config/1`.

apply/2
-------
And respectively `apply/1` which is the same as `apply/2` with empty args.

This function can be used to call all the functions that were registered to a certain callback, with the args given as seconds argument.

```erlang
eplugin:apply(my_fancy_callback, [1, 2, 3]).
```

callbacks/1
-----------
The basis for apply, this function returns all modules and functions registered for a certain callback. It returns: `[{Module, Function}]`

```erlang
eplugin:callbacks(my_fancy_callback).
%% -> [{some_module, some_function}, {some_other_module, some_function}]
 ```


config/1
--------
Fetches the custom plugin Config. This can is not tied to anything but simply can be used to store whatever plugin config is required.
```erlang
eplugin:config(my_fancy_callback).
%% -> {ok, Config}
 ```

Writing plugins
===============
A plugin is a directory with a `plugin.conf` file and one or more .erl files so the a plugin directory could look like:
```
-plugins
 -plug1
  -plugin.conf
  -plug1_test.erl
  -plug1_test2.erl
 -plug2
  -plugin.conf
  -plug2_test.erl
```

Please be aware that module names need to be unique!

The plugin.conf is a simple file with the following syntax:

```erlang
{PluginName,
 [{Module, [{Callback, Function}]}],
 OptionPlist}.
```