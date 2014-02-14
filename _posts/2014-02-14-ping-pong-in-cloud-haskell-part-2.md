---
layout: post
title:  "Hot Code Reloading in CH - Part 2"
tags: [thesis, erlang, cloud, haskell]
comments: true
share: true
image:
  feature: abstract-6.jpg
  credit: dargadgetz
  creditlink: http://www.dargadgetz.com/ios-7-abstract-wallpaper-pack-for-iphone-5-and-ipod-touch-retina/
---

In [Part 1] ({% post_url 2014-02-10-ping-pong-in-cloud-haskell %}), we talked about
the problems in trying to hot reload a CH module.

Before talking about our progress, I would like to mention a few problems in
plugins package. The plugins package is currently not maintained well and
does not work correctly with all recent versions of ghc. Particularly, module
loading does not work when a third party module is given as input on 64-bit
architecture. load requires the dependency information for loading all the
dependent modules in the correct order. This information is parsed from the
.hi files created when a haskell module is compiled. I am not sure why it is
unable to resolve objects in 64 bit architecture. Maybe the [hi parsing code
is broken](http://hub.darcs.net/stepcut/plugins) for 64 bit architecture. So,
it probably will only work on 32 bit architecture for now. I am testing the
code on a 32-bit Vagrant VM. Let me know if you want the VM image for testing.

Also, in ghc-7.6,
[unload](http://hackage.haskell.org/package/plugins-1.5.4.0/docs/src/System-Plugins-Load.html#unload)
does not actually unload the object code. It has been
[fixed in ghc-7.8 by Simon Marlow](http://www.reddit.com/r/haskell/comments/1le4y5/the_haxl_project_at_facebook_slides_from_my_talk/cbyoulm)
for static builds.

In my [previous post]({% post_url 2014-02-10-ping-pong-in-cloud-haskell %}),
I talked about the problem of doing an unload before a reload otherwise a
reload won't really do a load. In ghc-7.6, an unload does not unload the
Object code anyways. It simply removes the module from the list of currently
loaded modules. And load checks whether a module is already loaded by looking
at that list and does not reload if it already exists. Now, one way we could
prevent changing the type of DynamicT to include a Module type as
argument(which was a problem) is that we could simply call unload m just
after we load it. The next call to load will work perfectly, the current load
is still valid because unload didn't really unload the object code and we
don't need to explicitly pass the module to reboot.

Now, it works except that it still executes the old version. The reboot
function reloads the new version of code and simply returns. The return goes
back to the old code(still in memory). This means that load can handle
multiple versions of module in memory. But, instead of returning to the old
version of code, we would want to start with the new version of code.

If you look at the type of load,

{% highlight haskell linenos=table %}
load :: FilePath->[FilePath]->[PackageConf]->Symbol->IO (LoadStatus a)
{% endhighlight %}

you can pass a symbol and when you evaluate the value associated with that
symbol, you evaluate the code of that symbol (for example main).

One way to approach the problem would be the following. Instead of reboot
just loading a new version of code and returning, we don't let reboot
return. Instead we evaluate the new symbol.

If the symbol is main, we basically start from the beginning of the new
version (main) and we can short-circuit all initialization if it's reboot
instead of a boot. But we would need to pass all the state of the processes
as well as their hidden state such as mailbox(CQueue) which would require
lots of changes to the existing CH infrastructure.

If the symbol is instead "server", the process that we are upgrading, it
should not type check because the type of reboot is IO() and the type of server is (a
-> Process ()) but it does not give any type error. Any it runs perfectly
with the old vesion of the code as if the call to server at the end of reboot
has failed silently. I am not sure what is happening here.

If instead of evaluating server inside reboot, we could return the value to
server and let the server call the new server, that would be ideal just like
in erlang. But this does not look possible.

The type of reboot would be something like

{% highlight haskell linenos=table %}
reboot :: IO (Maybe a)
{% endhighlight %}

where a = DynamicT -> Process ()

The type of DynamicT is same as the type of reboot. So, the type of DynamicT
becomes

{% highlight haskell linenos=table %}
type DynamicT = IO (Maybe (DynamicT -> Process ()) )
{% endhighlight %}

This gives a "Cycle in type synonym declarations" type error.
Even if it type checked , calling the new server value from the old server
code, might not magically lead to sharing of process state between the old
and new "server".

Is there any hope to achieve erlang style ?MODULE:server call to simply
switch to the new version in memory flawlessly (with access to old mailbox which is
decoupled from the process in erlang) in Cloud Haskell? For now, it does not
look like it would be possible without fundamentally ripping apart the
Process state and radically changing the CH code.
