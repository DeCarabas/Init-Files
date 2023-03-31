if test -d /opt/local/bin
   set PATH /opt/local/bin $PATH
end
if test -d /opt/local/sbin
   set PATH /opt/local/sbin $PATH
end
if test -d ~/bin
   set PATH ~/bin $PATH
end
if test -d ~/.cargo/bin
   set PATH ~/.cargo/bin $PATH
end
if test -d ~/.local/bin
   set PATH ~/.local/bin $PATH
end
if test -d ~/devtools/buck/bin
   set PATH $PATH ~/devtools/buck/bin
end
if test -d /snap/bin
   set PATH $PATH /snap/bin
end
if test -d ~/go/bin
   set PATH $PATH ~/go/bin
end
if test -d ~/.local/bin
   set PATH $PATH ~/.local/bin
end
if test -d /nix
   set PATH $PATH /nix/var/nix/profiles/default/bin
end
if test -d $HOME/Library/Python/3.10/bin
   set PATH $PATH $HOME/Library/Python/3.10/bin
end

if test -n "$CODER_WORKSPACE_ID$CODER"
  if test -z "$BROWSER"
     set -x BROWSER "fwd-browse"
  end
end

if test -d /home/linuxbrew/.linuxbrew
  set -x HOMEBREW_PREFIX "/home/linuxbrew/.linuxbrew"
  set -x HOMEBREW_CELLAR "/home/linuxbrew/.linuxbrew/Cellar"
  set -x HOMEBREW_REPOSITORY "/home/linuxbrew/.linuxbrew/Homebrew"
  set PATH "/home/linuxbrew/.linuxbrew/bin" "/home/linuxbrew/.linuxbrew/sbin" $PATH
  set MANPATH "/home/linuxbrew/.linuxbrew/share/man" $MANPATH
  set INFOPATH "/home/linuxbrew/.linuxbrew/share/info" $INFOPATH
end

if [ -n "$INSIDE_EMACS" ]
  # This is here to make emacs and ansi-term work properly; I'm not *quite*
  # sure what it does but it's probably cool.
  function fish_title
    true
  end

  # emacs dir tracking
  function prompt_AnSiT -e fish_prompt
    printf "\eAnSiTc %s\n" "$PWD"
  end
  printf "\eAnSiTu %s\n" "$USER"
end
