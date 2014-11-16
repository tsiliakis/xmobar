#### **TL;DR**
 If you're just searching for the plugins I wrote, they are here :

 - [AptUpdates](https://github.com/tsiliakis/xmobar-test/blob/master/src/Plugins/AptUpdates.hs "AptUpdates")
 - [Countdown](https://github.com/tsiliakis/xmobar-test/blob/master/src/Plugins/Countdown.hs)
 - [LoadAverage](https://github.com/tsiliakis/xmobar-test/blob/master/src/Plugins/LoadAverage.hs)
 - [ShowIP](https://github.com/tsiliakis/xmobar-test/blob/master/src/Plugins/ShowIP.hs)



----------

<h1 class="sectionedit1" id="xmobar">xmobar</h1>
<div class="level1">

<p>
On this page I try to gather some information about <strong>xmobar</strong>, which is a status bar written in <a href="http://en.wikipedia.org/wiki/Haskell_(programming_language)" class="interwiki iw_wp" title="http://en.wikipedia.org/wiki/Haskell_(programming_language)">Haskell</a>. Here is the “about”-part from the <a href="http://projects.haskell.org/xmobar/#about" class="urlextern" title="http://projects.haskell.org/xmobar/#about"  rel="nofollow"> official website</a>.
</p>
<pre class="code">xmobar is a minimalistic, mostly text based, status bar. It was originally designed and implemented by 
Andrea Rossato to work with xmonad, but it&#039;s actually usable with any window-manager. xmobar was inspired 
by the Ion3 status bar, and supports similar  features, like dynamic color management, icons,
output templates, and extensibility through plugins.</pre>

<p>
Xmobar can be used with any window manager or desktop environment. You can use it with <a href="http://xmonad.org/" class="urlextern" title="http://xmonad.org/"  rel="nofollow">xmonad</a>, which is a excellent minimalistic window manager also written in Haskell (<a href="http://projects.haskell.org/xmobar/xmobar-xmonad.png" class="urlextern" title="http://projects.haskell.org/xmobar/xmobar-xmonad.png"  rel="nofollow">screenshot</a>), Gnome, KDE or as I do - with XFCE. 
</p>

<p>
Installing xmobar should be quite painless with your distribution&#039;s package manager, but can also be done via Cabal or the source code on <a href="https://github.com/jaor/xmobar" class="urlextern" title="https://github.com/jaor/xmobar"  rel="nofollow">Github</a>. Again the official website has excellent documentation on how each method works. I recommend on playing around with xmobar at first with your package managers version, but would switch to a more up-to-date version via cabal or source code, since then you can implement your own plugins and maybe even the plugins I wrote.
</p>

</div>
<!-- EDIT1 SECTION "xmobar" [1-1439] -->
<h2 class="sectionedit2" id="plugins">Plugins</h2>
<div class="level2">

</div>
<!-- EDIT2 SECTION "Plugins" [1440-1460] -->
<h3 class="sectionedit3" id="writing_your_own_plugin">Writing your own plugin</h3>
<div class="level3">

<p>
If you want to write your own plugin, the first stop should be the HelloWorld example (if you want the full source code, just click on HelloWorld.hs in the lower left corner of this snippet). This plugin implements the most basic functionality, that a plugin should have. As you can see, you can have your own plugin in 6 lines of code.
</p>

<p>
<script src="http://gist-it.appspot.com/github/jaor/xmobar/blob/master/samples/Plugins/HelloWorld.hs?slice=15:24&footer=minimal"></script>
</p>

<p>
I won&#039;t go into details about how to write a plugin, since the <a href="http://projects.haskell.org/xmobar/#writing-a-plugin" class="urlextern" title="http://projects.haskell.org/xmobar/#writing-a-plugin"  rel="nofollow"> official site</a> again does a great job at explaining it. On thing I will say to writing plugins is, that I recommend to start small. Have a rought idea of what you want to achieve and write it down in a simple .hs file. I wouldn&#039;t start immediately with the plugin, but first make sure, that your code is working and more importantly, working the way you intend to. Run it through ghci and play around with it, or even better - write tests. If you have a working .hs file , making it into a plugin is just adding a few lines at the top and the bottom.
</p>

</div>
<!-- EDIT3 SECTION "Writing your own plugin" [1461-2659] -->
<h3 class="sectionedit4" id="using_your_plugins">Using your plugins</h3>
<div class="level3">

<p>
In this section I will explain how to make XMobar recognize your plugins. As an example I use two of my own plugins : <strong><code>ShowIP</code></strong> and <strong><code>Countdown</code></strong>. Wherever you find those two names, you can just replace them with the name of your own plugin.For XMobar to be able to find your plugins, you have to place them in the right folder. The most convenient place is <code>src/Plugins</code>. Just copy your completed plugin into that folder. Next, you have to import your plugins into xmobar. This is done in two files.
</p>

</div>
<!-- EDIT4 SECTION "Using your plugins" [2660-3202] -->
<h5 class="sectionedit5" id="src_confighs">./src/Config.hs</h5>
<div class="level5">

<p>
Here you have to import your module. You can see that I just wrote my own plugins under the default plugins. Of course you can write yours at the top, but be sure to mark them in some way, so you can find them again. :)
</p>
<dl class="file">
<dt><a href="/doku.php?do=export_code&amp;id=xmobar&amp;codeblock=0" title="Download Snippet" class="mediafile mf_hs">src/Config.hs</a></dt>
<dd><pre class="code file haskell"><span class="br0">&#123;</span><span class="sy0">...</span><span class="br0">&#125;</span>
&nbsp;
<span class="kw1">import</span> Plugins<span class="sy0">.</span>Kbd
<span class="kw1">import</span> Plugins<span class="sy0">.</span>Locks
<span class="co1">-- these are my plugins that I wrote and I add them here</span>
<span class="kw1">import</span> Plugins<span class="sy0">.</span>ShowIP
<span class="kw1">import</span> Plugins<span class="sy0">.</span>Countdown
&nbsp;
<span class="br0">&#123;</span><span class="sy0">...</span><span class="br0">&#125;</span></pre>
</dd></dl>

<p>
The second thing you have to do, while you&#039;re in <code>Config.hs</code>, is to add your plugin data type to the runnableTypes list data type. ( Notice the <strong><em>:*:</em></strong> at the end )
</p>
<dl class="file">
<dt><a href="/doku.php?do=export_code&amp;id=xmobar&amp;codeblock=1" title="Download Snippet" class="mediafile mf_hs">src/Config.hs</a></dt>
<dd><pre class="code file haskell"><span class="br0">&#123;</span><span class="sy0">...</span><span class="br0">&#125;</span>
&nbsp;
runnableTypes <span class="sy0">::</span> Command :<span class="sy0">*</span>: Monitors :<span class="sy0">*</span>: Date :<span class="sy0">*</span>: PipeReader :<span class="sy0">*</span>: BufferedPipeReader :<span class="sy0">*</span>: CommandReader :<span class="sy0">*</span>: StdinReader :<span class="sy0">*</span>: 
XMonadLog :<span class="sy0">*</span>: EWMH :<span class="sy0">*</span>: Kbd :<span class="sy0">*</span>: Locks :<span class="sy0">*</span>: ShowIP :<span class="sy0">*</span>: Countdown :<span class="sy0">*</span>:
&nbsp;
<span class="br0">&#123;</span><span class="sy0">...</span><span class="br0">&#125;</span></pre>
</dd></dl>

</div>
<!-- EDIT5 SECTION "./src/Config.hs" [3203-4057] -->
<h5 class="sectionedit6" id="xmobarcabal">xmobar.cabal</h5>
<div class="level5">

<p>
If you used some external modules in your project, the right place to import them is the <code>xmobar.cabal</code>-file in the root directory. Just append the package name of your module and the appropriate version. If you&#039;re unsure what version to use, you can identify the function(s) you are using and find the oldest version where they function the way you want. If you are more in a hurry, or just want to test things, you can just put in the version you are currently using. An easy way to find out the version is the command <code>cabal info &lt;package-name&gt;</code> and the field <code>Versions installed : </code>.
</p>
<dl class="file">
<dt><a href="/doku.php?do=export_code&amp;id=xmobar&amp;codeblock=2" title="Download Snippet" class="mediafile mf_cabal">xmobar.cabal</a></dt>
<dd><pre class="code file haskell"><span class="br0">&#123;</span><span class="sy0">...</span><span class="br0">&#125;</span>
&nbsp;
   build<span class="sy0">-</span>depends:
      base <span class="sy0">==</span> <span class="nu0">4</span><span class="sy0">.*,</span>
      containers<span class="sy0">,</span>
      process<span class="sy0">,</span>
      old<span class="sy0">-</span>locale<span class="sy0">,</span>
      bytestring<span class="sy0">,</span>
      directory<span class="sy0">,</span>
      unix<span class="sy0">,</span>
      time<span class="sy0">,</span>
      filepath<span class="sy0">,</span>
      X11 <span class="sy0">&gt;=</span> 1<span class="sy0">.</span>6<span class="sy0">.</span>1<span class="sy0">,</span>
      mtl <span class="sy0">&gt;=</span> <span class="nu0">2.0</span> <span class="sy0">&amp;&amp;</span> <span class="sy0">&lt;</span> <span class="nu0">2.2</span><span class="sy0">,</span>
      parsec <span class="sy0">==</span> <span class="nu0">3.1</span><span class="sy0">.*,</span>
      stm <span class="sy0">&gt;=</span> <span class="nu0">2.3</span> <span class="sy0">&amp;&amp;</span> <span class="sy0">&lt;</span> <span class="nu0">2.5</span><span class="sy0">,</span>
<span class="co1">-- manually added dependency for my plugins</span>
      HTTP <span class="sy0">&gt;=</span> 4000<span class="sy0">.</span>0<span class="sy0">.</span>5
&nbsp;
<span class="br0">&#123;</span><span class="sy0">...</span><span class="br0">&#125;</span></pre>
</dd></dl>

</div>
<!-- EDIT6 SECTION "xmobar.cabal" [4058-5056] -->
<h3 class="sectionedit7" id="building_your_plugins">Building your plugins</h3>
<div class="level3">

<p>
The easiest way to build your own plugins is by using <a href="http://en.wikipedia.org/wiki/Cabal_(software)" class="interwiki iw_wp" title="http://en.wikipedia.org/wiki/Cabal_(software)">cabal</a>. If you checked out the git repo, you just have to go into the directory with the <code>xmobar.cabal</code> and execute the following command :
</p>
<pre class="code">cabal configure --enable-benchmarks --enable-tests --verbose=2 &amp;&amp; cabal build --verbose=2 &amp;&amp; cabal install --haddock-executables</pre>

<p>
This command creates also the  <a href="http://en.wikipedia.org/wiki/Haddock_(software)" class="interwiki iw_wp" title="http://en.wikipedia.org/wiki/Haddock_(software)">Haddock</a> documentation and the verbosity level is increased. Also dependency checking, compilation for test suites and compilation for benchmarks is activated. 
</p>

</div>
<!-- EDIT7 SECTION "Building your plugins" [5057-5669] -->
<h2 class="sectionedit8" id="my_plugins">My Plugins</h2>
<div class="level2">

</div>
<!-- EDIT8 SECTION "My Plugins" [5670-5693] -->
<h4 class="sectionedit9" id="showip">ShowIP</h4>
<div class="level4">

<p>
A simple plugin to display the external IP or any other String from a webserver.
</p>

<p>
This plugin can be pointed to a website, retrieves its content with a Http GET and displays the string. It needs 3 parameters : a String, which is the prefix (e.g. &#039;IP : &#039;), a String which is the url of the server and a int which is the rate the plugin updates in tenths of seconds.
</p>

<p>
As a simple server that returns a IP, feel free to use my server at http:srv.u0.org/ip.php , but I can&#039;t guarantee that it will be up forever. An alternative is http:wtfismyip.com/text. 
</p>

</div>
<!-- EDIT9 SECTION "ShowIP" [5694-6263] -->
<h4 class="sectionedit10" id="countdown">Countdown</h4>
<div class="level4">

<p>
A simple plugin to display the remaining days till a specific date.
</p>

<p>
This plugin needs 5 parameters : The date&#039;s Year Month Day and a string that can be shown after the difference of days. The recommendet value for the string is &#039; Days&#039;. The last parameter is the update rate of the plugin in tenth of seconds.
</p>

<p>
It can be used as a countdown till e.g. New Years or vacations. If you set the date to New Years you automatically get the day of the year. 
</p>

</div>
<!-- EDIT10 SECTION "Countdown" [6264-6736] -->
<h4 class="sectionedit11" id="aptupdates">AptUpdates</h4>
<div class="level4">

<p>
A simple plugin to display the number of packages that can be updated via aptitude.
</p>

<p>
The plugin needs 2 parameters : a string to display after the number of updatable packages and the rate the plugin should check for new packages in tenth of seconds.
It&#039;s best when you have a cron-job to update the package database (aptitude update) and use this plugin to show the packages that are available to be updated. This plugin does not(!) automatically update your repository, since root rights are needed for that.
</p>

<p>
Since it uses aptitude, you have to run a system with aptitude. This plugin was tested on Debian Wheezy 3.2.0-4-amd64 and was build with aptitude 0.6.8.2 in mind. 
</p>

</div>
<!-- EDIT11 SECTION "AptUpdates" [6737-7433] -->
<h4 class="sectionedit12" id="loadaverage">LoadAverage</h4>
<div class="level4">

<p>
A simple plugin to display the load average of the system.
</p>

<p>
This plugin is using uptime to retrieve the load average and then parses the result to a definable form. It needs 2 paramters : a String that will be displayed before the load average (e.g. &#039;Load average :&#039;) and another String, which will be used as the delimiter. E.g. - , which will result in &#039;0.93 - 0.97 - 1.01&#039;. of course a simple space &#039; &#039; can also be used). 
</p>

</div>
<!-- EDIT12 SECTION "LoadAverage" [7434-7880] -->
<h2 class="sectionedit13" id="xmobar_configuration">xmobar configuration</h2>
<div class="level2">

<p>
To get xmobar running you need a config file in your home directory. This file is usually called <code>.xmobarrc</code>. Here you can fine tune your xmobar, configure which plugins to run and how to show them. xmmobar&#039;s config file is quite easy to modify and is more or less self explanatory. I will once again point to the <a href="http://projects.haskell.org/xmobar/#system-monitor-plugins" class="urlextern" title="http://projects.haskell.org/xmobar/#system-monitor-plugins"  rel="nofollow">official documentation</a>, but will attach my configuration that works with xmobar 0.18. Modification is maybe needed on the network interface (mine is eth0) and in order to run <code>Countdown, ShowIP, AptUpdates</code> and <code>LoadAverage</code> you have to compile my plugins.
</p>
<pre class="code haskell">Config <span class="br0">&#123;</span> font <span class="sy0">=</span> <span class="st0">&quot;-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*&quot;</span>
       <span class="sy0">,</span> borderColor <span class="sy0">=</span> <span class="st0">&quot;black&quot;</span>
       <span class="sy0">,</span> border <span class="sy0">=</span> TopB
       <span class="sy0">,</span> bgColor <span class="sy0">=</span> <span class="st0">&quot;black&quot;</span>
       <span class="sy0">,</span> fgColor <span class="sy0">=</span> <span class="st0">&quot;white&quot;</span>
       <span class="sy0">,</span> position <span class="sy0">=</span> Bottom
       <span class="sy0">,</span> lowerOnStart <span class="sy0">=</span> False
       <span class="sy0">,</span> persistent <span class="sy0">=</span> False
       <span class="sy0">,</span> hideOnStart <span class="sy0">=</span> False
       <span class="sy0">,</span> allDesktops <span class="sy0">=</span> True
       <span class="sy0">,</span> overrideRedirect <span class="sy0">=</span> True
       <span class="sy0">,</span> commands <span class="sy0">=</span> <span class="br0">&#91;</span> Run Network <span class="st0">&quot;eth0&quot;</span> <span class="br0">&#91;</span><span class="st0">&quot;-L&quot;</span><span class="sy0">,</span><span class="st0">&quot;0&quot;</span><span class="sy0">,</span><span class="st0">&quot;-H&quot;</span><span class="sy0">,</span><span class="st0">&quot;1600&quot;</span><span class="sy0">,</span><span class="st0">&quot;--normal&quot;</span><span class="sy0">,</span><span class="st0">&quot;green&quot;</span><span class="sy0">,</span><span class="st0">&quot;--high&quot;</span><span class="sy0">,</span><span class="st0">&quot;red&quot;</span><span class="sy0">,</span><span class="st0">&quot;-m&quot;</span><span class="sy0">,</span><span class="st0">&quot;4&quot;</span><span class="sy0">,</span><span class="st0">&quot;-t&quot;</span><span class="sy0">,</span><span class="st0">&quot;&lt;rx&gt;/&lt;tx&gt;&quot;</span><span class="br0">&#93;</span> <span class="nu0">10</span>
                    <span class="sy0">,</span> Run Memory <span class="br0">&#91;</span><span class="st0">&quot;-t&quot;</span><span class="sy0">,</span><span class="st0">&quot;Mem: &lt;used&gt; MB&quot;</span><span class="br0">&#93;</span> <span class="nu0">30</span>
                    <span class="sy0">,</span> Run Swap <span class="br0">&#91;</span><span class="br0">&#93;</span> <span class="nu0">1000</span>
                    <span class="sy0">,</span> Run Date <span class="st0">&quot;%A, %e.%0m %B - %_H:%M&quot;</span> <span class="st0">&quot;date&quot;</span> <span class="nu0">10</span>
                    <span class="sy0">,</span> Run MultiCpu <span class="br0">&#91;</span><span class="st0">&quot;-t&quot;</span><span class="sy0">,</span><span class="st0">&quot;CPU : &lt;bar&gt; &lt;total0&gt;% /&lt;total1&gt;%&quot;</span><span class="sy0">,</span><span class="st0">&quot;-L&quot;</span><span class="sy0">,</span><span class="st0">&quot;3&quot;</span><span class="sy0">,</span><span class="st0">&quot;-H&quot;</span><span class="sy0">,</span><span class="st0">&quot;50&quot;</span><span class="sy0">,</span><span class="st0">&quot;--normal&quot;</span><span class="sy0">,</span><span class="st0">&quot;green&quot;</span><span class="sy0">,</span><span class="st0">&quot;--high&quot;</span><span class="sy0">,</span><span class="st0">&quot;white&quot;</span><span class="sy0">,</span><span class="st0">&quot;-p&quot;</span><span class="sy0">,</span><span class="st0">&quot;3&quot;</span><span class="sy0">,</span><span class="st0">&quot;-b&quot;</span><span class="sy0">,</span><span class="st0">&quot; &quot;</span><span class="sy0">,</span><span class="st0">&quot;-f&quot;</span><span class="sy0">,</span><span class="st0">&quot;|&quot;</span><span class="br0">&#93;</span> <span class="nu0">10</span>
                    <span class="sy0">,</span> Run Countdown <span class="nu0">2013</span> <span class="nu0">7</span> <span class="nu0">11</span> <span class="st0">&quot; Days &quot;</span> <span class="nu0">36000</span>
                    <span class="sy0">,</span> Run ShowIP <span class="st0">&quot;IP : &quot;</span> <span class="st0">&quot;http://srv.u0.org/ip.php&quot;</span> <span class="nu0">6000</span>
                    <span class="sy0">,</span> Run AptUpdates <span class="st0">&quot; new Packages&quot;</span> <span class="nu0">36000</span>
                    <span class="sy0">,</span> Run LoadAverage <span class="st0">&quot;Load : &quot;</span> <span class="st0">&quot; &quot;</span> <span class="nu0">200</span>
                    <span class="br0">&#93;</span>
       <span class="sy0">,</span> sepChar <span class="sy0">=</span> <span class="st0">&quot;%&quot;</span>
       <span class="sy0">,</span> alignSep <span class="sy0">=</span> <span class="st0">&quot;}{&quot;</span>
       <span class="sy0">,</span> template <span class="sy0">=</span> <span class="st0">&quot;%multicpu% | %memory% * %swap% | %eth0% | %ShowIP% | %Countdown% | %AptUpdates% }{ %LoadAverage% - %date%&quot;</span>
       <span class="br0">&#125;</span></pre>

<p>
One thing I want to point out, is a specific error, that puzzled me at first :
</p>
<pre class="code">configuration file contains errors at:
&quot;Config&quot; (line 23, column 10):
unexpected &quot;s&quot;
expecting space or &quot;Run&quot;</pre>

<p>
This error is thrown by xmobar, when you change your config and the parameters for the plugins aren&#039;t right. Don&#039;t be confused by the line and column that are printed, this has to the with the fact, that the plugins aren&#039;t loaded properly and Haskell expects a parameter and finds the field sepChar. A good way to debug this, is to comment out the any line in question and start with a plain config.
</p>

</div>
<!-- EDIT13 SECTION "xmobar configuration" [7881-] -->
