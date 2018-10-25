---
title: "WSL Dev Setup"
date: 2018-10-24T11:54:35-07:00
draft: true
---

## Prereqs
Follow the guide [here and install WSL.](https://docs.microsoft.com/en-us/windows/wsl/install-win10) I chose Ubuntu 18.04 because it's got access to a large and well-curated package library and there are plentiful answers to problesm with it. It also seems to be the "default" distro that most people install on WSL and so instructions for getting tricky things working on WSL usually assume you're using Ubuntu.

## Terminology
This guide will refer to running/installing things on Windows as well as referring to installing things in the Ubuntu Linux guest system that was installed via the Windows Subsystem for Linux. For simplicity's sake, I will refer to these as the "Windows side" and "Linux side" respectively.

## Mounting source code

### Natural filesystem locations

I use a hybrid workflow where any GUI programs I use (mostly VS Code and IntelliJ) run in Windows and all command line tools (vim, git, mvn, etc) run in the WSL Ubuntu installation. Unfortunately, it's not possible to access files on the Linux side filesystem from the Windows side.

This created an annoying problem for me. I organize all of my code under a 'dev' directory in my home directory like so:
```
/home/psanford/dev
                  /sisu
                       /<work stuff>
                  /personal
                           /kubeleans
                           /pmsanford.github.io
                           ...more personal projects
                 /sandbox
                         /redshift-jdbc-test
                         /rustborrow
                         ...more throwaway projects to test things
```

It IS possible to access Windows files from the Linux side via, for ex, `/mnt/c` for the C: drive. So, to solve this, I created a `dev` folder in `C:\Users\me\` (my email is `me at paulsanford dot net` and so my windows user account name is just `me`) and symlinked `/mnt/c/Users/me/dev` to `~/dev`. (Actually, in my setup, it's `/c/Users/me/dev`. We'll get to why and how in the Docker section. Don't make any symlinks until after the next section.)

### Permissions
On the Windows 10 side, we're using NTFS. Obviously, there aren't Unix style permissions on the files there. By default, DrvFS (the filesystem driver Microsoft wrote to allow the Linux side to share files with Windows) sets the owner and group of all files on the Windows side to root. Additionally, chown and chmod are no-ops. Sounds bad! Fortunately, there's a fix.

You can create a file called `/etc/wsl.conf` and tell WSL to set some more sane options:

```ini
[automount]
root = /
options = "metadata,umask=22,fmask=11"
```

* **root**: This changes where the drives are mounted. So with root set this way, the `C:\` drive will be mounted at `/c` instead of `/mnt/c`. We'll see why this is desirable later.
* **options**:
    - **metadata**: This tells the DrvFS driver to keep track of permissions and ownership.
    - **umask=22**: For all files with no metadata associated (so things we haven't run `chmod` or `chown` on yet on the Linux side), set their permissions to 755 (read/write/execute for owner, read/execute for all others).
    - **fmask=11**: Similar to above, but without execute bits.

So now ownership and permissions work correctly in Linux, and the defaults are sane (the execute bits are set on directories because the execute bit means that you can list contents).

For this to take effect, you'll need to restart your Linux system. You can do that with the `wslconfig.exe` utility. First, shutdown all your Linux terminals. Then, **from a Powershell window on the Windows side,** run the following:
```shell
PS C:\> wslconfig /l
Windows Subsystem for Linux Distributions:
Ubuntu-18.04 (Default)
PS C:\> wslconfig /t Ubuntu-18.04
```

Now start your Linux distro from the start menu like usual. You shold be able to do `ls /c` and see all your Windows folders there. Note that after the root change, you'll need to update any symlinks you made before.

## Docker

One of the quirks of running WSL verses running Linux natively (or in a VM) is that there's no real `init` or `systemd`. This means that you'll have to do some tricks for things that would normally run as a daemon. Usually, there's a Windows version that you can run on the Windows side as a service and interact with from the command line on the Linux side over TCP. This is how we'll run Docker.

### Installing Docker for Windows

Follow the [instructions on the Docker site](https://docs.docker.com/docker-for-windows/install/) to install Docker for Windows (obviously, on the windows side). Once that's done, there's one setting you'll need to change:

![Image of Docker configuration](/images/docker-config.png)

Please use caution (meaning don't expose this port through the firewall).

### Installing Docker for Linux

Install docker using the [instructions on the Docker site](https://docs.docker.com/install/linux/docker-ce/ubuntu/) for your distro, but come back here before Step 4 (running the `hello-world` image). Make sure you use the Docker repos; The upstream repos for Docker are hopelessly out of date (for good reasons I won't go into), but we want the latest version to develop with.

Now, if you ignored what I said and tried to run the `hello-world` image, you'll notice it failed. That's because there's no init, there's no systemd, and consequently, there's no docker daemon running. We're going to connect the Linux docker *client* to the Windows docker *daemon*.

Run the following:
```shell
echo "export DOCKER_HOST=tcp://0.0.0.0:2375" >> ~/.bashrc && source ~/.bashrc
```
This is telling docker to connect to localhost over TCP rather than trying to connecting to a filesystem socket like it normally would on Linux. Now, let's try running the hello world image:
```shell
docker run hello-world
```

Thanks to [Nick Janetakis](https://nickjanetakis.com/) for a very helpful [blog post](https://nickjanetakis.com/blog/setting-up-docker-for-windows-and-wsl-to-work-flawlessly) that lays out how to do this comprehensibly (check it out if you're having problems with the above).

### Hey what about remounting C: at /c

Right, so this is a peculiar interaction between the two Linuxes that you're now running on your Windows machine. Docker for Windows installs a Linux virtual machine in which to run Docker (hit Start and run Hyper-V Manager; You'll see the MobyLinuxVM there. Moby the singer sponsors Docker so they'll name the VM after him.). Inside this VM, Docker mounts the C: drive at /c/ to facilitate volume mounting files into docker containers. So when you go to volume mount a file or directory into a docker container, you'll do it relative to `/c`. Setting the root to `/` like we did above aligns our WSL paths with the MobyLinuxVM paths. Neat, right? Personally, I also think it's nicer to have `C:\` under `/c` but you can do what you want.

## X-Windows, the clipboard, and you

One thing I absolutely need to be able to do is get the output of programs running on the command line into the clipboard, somehow. I started out with a hacky alias that I will not reproduce here which invoked powershell.exe to set the clipboard. A much better way, however, is to just install `xclip`. To do so, though, we need an X server running. The best Windows X server, in my opinion, is [VcXsrv](https://sourceforge.net/projects/vcxsrv/). It's based on the xorg sources, but compiled with MSVC. Grab it from SourceForge and install it.

Once you've got that installed, run `XLaunch` in Windows. You can go ahead and take all the defaults in the launch wizard.

Next, run the following on the Linux side:
```shell
sudo apt-get install xclip
ls /proc | xclip
```

Now open up Notepad on the Windows side and hit Control+V. You should see your directory listing appear there. Nice, now your clipboard can be shared across the win/lin boundary. Keep in mind that your X server has to be running for this to work.

#### I unplugged my external monitor and now my Linux GUI program is stuck in hell forever

VcXsrv doesn't handle unplugging monitors with windows on them very well. Fortunately, if you right click the X icon in the taskbar and click "Gather Windows," they should all reappear on your current monitor

#### I have a high DPI monitor and I can't see shit in these X windows

Yeah that sucks. This isn't a WSL specific problem for what it's worth. you can try setting `GDK_SCALE=2` and depending on the app this might work.

## This terminal sucks

My main gripe with the built in terminal emulator that ships with WSL, namely that it doesn't support Vim cursor modes, is fixed in the latest insider builds of Windows, which upgrades it from "unusable" to "passable." If you're unwilling to live on the edge in developer preview (or just want more features from your term) here are some options I'd recommend:

### Run your favorite Linux console through XWindows
You can install and run something like [Terminator](https://terminator-gtk3.readthedocs.io/en/latest/) (which is my favorite) directly from the Linux side using the X Server you installed above. This works pretty well, but the font rendering isn't great, and seems to degrade under some conditions I can't quite figure out.

### Run a Windows term that points at bash.exe
You can run some of the Windows console replacements like [Cmder](http://cmder.net/) and [point them at bash.exe](https://gingter.org/2016/11/16/running-windows-10-ubuntu-bash-in-cmder/) as a shell. This is a great option if you also use PowerShell a lot; You can have a uniform interface.

There's also [Alacritty](https://github.com/jwilm/alacritty), which supports Windows now and is my current favorite choice.

The main problem with running Windows terms is, until the change makes it into a mainline Windows release, they also do not support the changing cursor based on mode in Vim.
