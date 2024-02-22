fish_add_path ~/.local/bin \
              ~/.cargo/bin \
              ~/bin \
              /opt/local/sbin \
              /opt/local/bin

fish_add_path --append \
              ~/devtools/buck/bin \
              /snap/bin \
              ~/go/bin \
              /nix/var/nix/profiles/default/bin \
              ~/Library/Python/3.10/bin \
              /opt/awscli/bin

if command -s pyenv > /dev/null
   set -Ux PYENV_ROOT $HOME/.pyenv
   fish_add_path --move $PYENV_ROOT/bin

   pyenv init - | source
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
