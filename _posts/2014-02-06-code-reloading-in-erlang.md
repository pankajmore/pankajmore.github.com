---
layout: post
title:  "Do messages get lost when erlang modules are upgraded?"
tags: [thesis,erlang]
image:
  feature: abstract-6.jpg
  credit: dargadgetz
  creditlink: http://www.dargadgetz.com/ios-7-abstract-wallpaper-pack-for-iphone-5-and-ipod-touch-retina/
comments: true
share: true
---

One of the problems that I am currently dealing with in my thesis is
the following:

How to make sure that the messages arriving
during an upgrade don't get dropped?

First , we have to make sure what happens in erlang. According to the
erlang manual, erlang keeps 2 versions of the module in memory i.e old
and current.  When a module is upgraded,

* the old version is discarded
* processes running the old version crash(or maybe are killed)
* the current version is marked as old
* the new version becomes current

Fully qualified function calls of the from ?MODULE:foo() always refer
to the current version.  Non qualifited function calls such as foo()
refer to the version in which they were invoked.

So, code reloading boils down to simple using a fully qualified function call!

Coming back to the original question, what happens to the messages in
transit during an upgrade. Below is my attempt at figuring out the
answer.

{% highlight erlang linenos=table %}
-module('pingpong').
-compile(export_all).
-import(timer,[sleep/1]).


start(N) ->
    Server = spawn(?MODULE,server,[]),
    _ = spawn(?MODULE,client,[Server,N,0]).

server() ->
    receive
        upgrade ->
            compile:file(?MODULE),
%%            sys:suspend(?MODULE),
            code:purge(?MODULE),
            sleep(1000),
            code:load_file(?MODULE),
%%            sys:resume(?MODULE),
            ?MODULE:server();
        {ping,Cid} ->
            sleep(1000),
%%            io:format("New version Running!~n"),
%%            io:format("Received a PING!~n"),
            Cid ! pong,
%%            io:format("Sent        a PONG!~n"),
            server()
    end.

client(_,0,C) ->
    io:format("DONE!~n"),
    io:format("Received ~p PONGS!~n",[C]);
client(Server,5,C) ->
    io:format("Sending an upgrade message!~n"),
    Server ! upgrade,
    From = self(),
    Server ! {ping,From},
    io:format("Sent          a PING!~n"),
    receive
        pong ->
            io:format("Received a PONG!~n"),
            client(Server,5-1,C+1)
    end;
client(Server,N,C) ->
    sleep(1000),
    From = self(),
    Server ! {ping,From},
    io:format("Sent          a PING!~n"),
    receive
        pong ->
            io:format("Received a PONG!~n"),
            client(Server,N-1,C+1)
    end.
{% endhighlight %}


You can try changing the code before an upgrade message is received
and see for real that hot-code reloading indeed works!  Also, since
the client is sending 10 pings, it must receive 10 pongs. Does this
guarantee that no pings are lost?

Here is a demo in my terminal showing the execution of the above
program. During the execution before the 5th ping, I uncomment the
code at line 29 to display the new version running message after an
upgrade has occurred.

<iframe src="http://showterm.io/0159350f205cc475c7f18" width="640" height="600"></iframe>

Compared to Haskell, Erlang has a vm which is a huge advantage since
it controls the execution and keeps multiple versions of modules in
memory.  Besides, it can make sure that some calls go to the current
module and some go to the old module.  Also, the mailbox of each
process is preserved during upgrades as it is separately managed by the vm.

How to achieve all this in Cloud Haskell?
Why do we need to have mutiple versions of modules in memory?
Think about it! I will answer this in my next post.
