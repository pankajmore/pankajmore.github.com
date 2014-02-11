---
layout: post
title:  "Hot Code Reloading in Cloud Haskell?"
categories: thesis erlang cloud haskell
comments: true
---

We want to achieve message passing reliability as in case of Erlang
during a code upgrade.  In an [earlier post] ({% post_url 2014-02-06-code-reloading-in-erlang %}) , we showed a simple test to
check if messages get lost in the ether while an upgrade is taking
place.

We would ideally like that cloud haskell also does not lose
messages while it upgrades our code from one version to the next.
In the worst case, we could make do without this requirement since
messages can anyways get lost due to physical disconnects and the
supervisor mechanism along with fault tolerance techniques like link
and monitor can be used to make sure that faults get isolated. In a
way, we can defer the problem to the unreliability of physical world
while an upgrade is in place.

One important issue in haskell is that we cannot easily have multiple
versions of the same module in memory using plugins package. It might
require ghc runtime support but not much research has been done in
that direction. Even without this support, we still could have
multiple processes running on a single node and all of them might get
upgraded together since all refer to the same module in memory. Or
some processes might crash. Best way to know is by trying it out.

Below is a simple implementation of Ping Pong system in cloud
haskell. Instead of a special message type for upgrade, we simply
trigger an upgrade when we receive a particular integer.

{% highlight haskell linenos=table %}

{-# LANGUAGE DeriveDataTypeable,ScopedTypeVariables #-}
module PingPong where
import Control.Concurrent ( threadDelay )
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport ( closeTransport )
import Network.Transport.TCP

server :: DynamicT -> Process ()
server st = do
    (cid,x) :: (ProcessId,Int) <- expect
    liftIO $ putStrLn $ "Got  a Ping with value : " ++ (show x)
    case x of
      4 -> do
        liftIO $ putStrLn $ "UPGRADE"
        liftIO $ st
      _ -> do
        send cid x
        liftIO $ putStrLn $ "Sent a Pong with value : " ++ (show x)
        server st

client :: DynamicT -> Int -> ProcessId -> Process ()
client st 10 sid = do
  liftIO $ putStrLn "DONE"
client st c sid = do
  me <- getSelfPid
  send sid (me,c)
  liftIO $ putStrLn $ "Sent a Ping with value : " ++ (show c)
  (v :: Int) <- expect
  liftIO $ putStrLn $ "Got  a Pong with value : " ++ (show v)
  client st (c+1) sid

ignition :: DynamicT -> Process ()
ignition st= do
    -- start the server
    sid <- spawnLocal $ server st
    -- start the client
    cid <- spawnLocal $ client st 0 sid
    return ()
    liftIO $ threadDelay 100000-- wait a while

type DynamicT = IO ()

main :: DynamicT -> IO ()
main st = do
    Right transport <- createTransport "127.0.0.1" "8080"
                            defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node $ ignition st
    closeTransport transport

{% endhighlight %}

Here is the static core which is responsible for running and reloading
our dynamic application. The code in static core is pretty trivial
and boilerplate. It simply compiles, loads the PingPong module and
evaluates main of PingPong by passing it an argument of type DynamicT
which contain everything that is required for dynamic reloading. In
this example, we are not concerned with state preservation or state
migration, so we only pass the reboot function to the dynamic core.

{% highlight haskell linenos=table %}
reboot :: IO ()
reboot = forever $ do
  putStrLn "Loading"
  r <- makeAll "PingPong.hs" []
  case r of
    MakeSuccess mc fp -> do
      mv <- load fp [] [] "main"
      putStrLn $ show $ mc
      putStrLn "Loaded"
      case mv of
        LoadFailure msgs -> putStrLn "fail" >> print msgs
        LoadSuccess m v -> do
        putStrLn "success"
        v reboot
        unloadAll m
    MakeFailure msgs -> putStrLn "failed to make" >> print msgs
  putStrLn "Press y to reload"
  getChar

type DynamicT = IO ()

main :: IO ()
main = do
  putStrLn "Loading"
  r <- makeAll "PingPong.hs" []
  case r of
    MakeFailure msgs -> putStrLn "failed to make" >> print msgs
    MakeSuccess mc fp -> do
      mv <- load fp [] [] "main"
      putStrLn $ show $ mc
      putStrLn "Loaded"
      case mv of
        LoadFailure msgs -> putStrLn "fail" >> print msgs
        LoadSuccess m v -> do
        putStrLn "success"
        s <- v reboot
        getChar
        return ()

{% endhighlight %}


Why did we define reboot in the static module?Why can't we simple
define a reboot function in the dynamic module?

reboot function will require load,unload and make from System.Plugins
modules which need to be imported in the dynamic module. This does not
work in practice which means we can't import plugins in the dynamic
module. Why not? The plugins module contains the code for dynamically
reloading. If it is imported in the dynamic module also, the static
core will try to recursively load and unload the plugins module
also. If the code for plugins module is unloaded, then the dynamic
reloading cannot work. So, it does not make sense to try to
load/unload plugins as a module in dynamic module.

We cannot write our reboot function in dynamic module. So the only
other place, we can write it is in the static module. But this reboot
function must be executed inside the process which wants to
upgrade. So, what do we do? We pass this reboot function as a value to
all processes which might need to upgrade themselves.

So, all is well and good and the code above should work right? Not so
fast bro! Can you spot the problem in the code? Take your time!

Well the first subtle bug is that reboot function is incorrect. The
dynamic module must be unloaded before a load otherwise it still keeps
the old version in the memory. Basically, load doesn't really load if
the module already exists in memory even though the version in memory
is old. We must unload m(which basically removes the entry that m is
loaded) before the load. To really do that, we need to know which
module m should we unload. And that information is only available
after a load has occurred! So, we tried doing a useless load and then
getting hold of the module m, we unload m  and then again do the final
real load which should work perfectly. It does not work in practice!
The hack is very ugly, since we don't want to load twice.
The ideal solution would be to pass the module that we need to unload
as an argument to reboot. So the type of reboot then becomes

{% highlight haskell %}
reboot :: Module -> IO ()
{% endhighlight %}

But who knows what module needs to be reloaded. The static core only
knows it(while it boots up the dynamic module). But, reboot needs to
be called by the dynamic module. So, static core should communicate
this value of m to the dynamic module. How? It can simply send this
value while calling the main of dynamic module.

The type of main of dynamic module becomes :
{% highlight haskell %}
DynamicT :: (Module,Module->IO())
{% endhighlight %}

But the type of DynamicT needs to be also declared in the dynamic
module. Declaring the new type of DynamicT in dynamic module is not
possible since that would required importing the plugins module(since
it exports the Module constructor).We cannot import plugins module in
dynamic module without doing some hackery as discussed above.
One possible way to go about it might be to change the unload function
to no op in case it want to unload the plugins package. That would
allow importing the plugins package in the dynamic module.

What other reasons might be against loading the plugins module in
dynamic module? Is there some other way to do an unload before a load
in the reboot function?

Once the reboot function is fixed, we have whole host of other
problems such as

* properly returning to the correct location in the new version of
code
* making sure that message queues don't get wiped out when the upgrade
  occurs
* making sure that threads responsible for managing the queue don't
  get killed and continue to receive message from the network buffers
* upgrading one process does not crash other processes running on the
  same node
* how to manage the state problem in case the state of processes need
  to be preserved across upgrades

Currently, we need answers to the following questions before we can
look to fixing the above problems?

* What happens to message queues and other state created by processes
  during an upgrade?
* What happens to the threads which are running the processes themselves during
  an upgrade? Are the processes(threads) killed? If not, will they
  continue to work without problems after reboot?

Thoughts and comments please!
