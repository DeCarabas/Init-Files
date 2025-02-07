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

# Somehow fish does the wrong thing for tmux in 24bit color mode.
# or tmux doesn't do the right thing with the low palette colors
# or something anyway we re-map the colors explicitly for tmux.
# These colors come from vscode dark+
if test "$TERM" = "tmux-direct"
  set -g fish_color_autosuggestion 666666
  set -g fish_color_cancel -r
  set -g fish_color_command 396ec7
  set -g fish_color_comment c62f37
  set -g fish_color_cwd 37be78
  set -g fish_color_cwd_root c62f37
  set -g fish_color_end 37be78
  set -g fish_color_error e94a51
  set -g fish_color_escape 49b7da
  set -g fish_color_history_current --bold
  set -g fish_color_host normal
  set -g fish_color_host_remote e2e822
  set -g fish_color_normal normal
  set -g fish_color_operator 49b7da
  set -g fish_color_param 3ba7cc
  set -g fish_color_quote e2e822
  set -g fish_color_redirection '3ba7cc'  '--bold'
  set -g fish_color_search_match 'f2f84a'  '--background=666666'
  set -g fish_color_selection 'e5e5e5'  '--bold'  '--background=666666'
  set -g fish_color_status c62f37
  set -g fish_color_user 45d38a
  set -g fish_color_valid_path --underline
end
