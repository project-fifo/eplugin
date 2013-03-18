eplugin
========
This library intends to provide a simple way to manage plugins for an Erlang application, right now the abstraction is very simple and high level but it is quite flexible.

usage
-----

Simply start the application, there is only one configuration variable at the moment named `plugin_dir` which as the name suggests specifies where the plugins are discovered. It defaults to `"plugins"`.

apply/2
-------
This function can be used to call all the functions that were registered to a certain callback, with the args given as seconds argument.

```erlang
eplugin:apply(my_fancy_callÂback, [1, 2, 3]).
```

call/1+
-------
This function is a simplification of apply with a fixed number of arguments, this way it gets around using `erlang:apply` internally.

```erlang
eplugin:apply(my_fancy_callback, [1, 2, 3]) =:= eplugin:call(my_fancy_callback, 1, 2, 3).
```

fold/2
------
This function folds an argument through all callbacks. The first argument is the name the second the Acc0 for the fold.

test_apply/1+
------
Threads through the callbacks as long as true is returned, if anything else is returned execution is terminated and this value returned.

test_apply/2  test/1+
---------------------
Threads through the callbacks as long as true is returned, if anything else is returned execution is terminated and this value returned.

Naming is according to apply and call.

callbacks/1
-----------
The basis for apply, this function returns all modules and functions registered for a certain callback. It returns: `[{Module, Function}]`

```erlang
eplugin:callbacks(my_fancy_callback).
%% -> [{some_module, some_function}, {some_other_module, some_function}]
 ```

config/1
--------
Fetches the custom plugin Config. This is holding plugin internal information as well as some used by eplugin.project-fifo.net

```erlang
eplugin:config(my_fancy_callback).
%% -> {ok, Config}
 ```

plugins/0
---------
This function lists all installed plugins.

enable/1
--------˘
Enables a plugin.

disable/1
---------
disables a plugin.

is_enabled/1
------------
Returns true if a plugin is enabled (aka has any callbacks registered).

register/4 /5
-------------
Registers a callback.


Writing plugins
===============
A plugin is a directory with a `plugin.conf` file and one or more .erl files so the plugin directory could look like:
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
 [{Module, [{Callback, Function}|{Callback, Function, CallbackOptions}]}],
 OptionPlist}.
```

CallbackOptions has the following possible values
* priority - the priority for execution order, highest priority first, default is 0

OptionsPlist has the following reserved options:

* disabled - this plugin will not load.
* dependencies - a list of dependencies.¯
* provides - a list of dependencies the plugin provides.
* add path - relative pathes to be added to the loadpath.
Callbacks
=========

notation
--------
callbacks are noted as `<callback name>(arguments)` so `eplugin:init(Config)` means the callback `eplugin:init` is called with 1 argument - `Config`.

internal callbacks¯¯
------------------
eplugin provides the following callbacks itself:
* eplugin:enable(Config) - this is called before a module gets enabled.
* eplugin:disable(Config) - this gets called after a module gets disabled.
* eplugin:enable_plugin(Plugin) - this gets called whenever a plugin is enabled.
* eplugin:disable_plugin(Plugin) - this gets called whenever a plugin is disabled.