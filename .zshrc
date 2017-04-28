source ~/.antigen-source/antigen.zsh

alias elm='nocorrect elm'
alias ping='nocorrect ping'
alias status='git status'
alias commit='git commit -am'
alias push='git push origin'
alias pull='git pull origin'
alias codemod='codemod.py'
alias reload='entr'
alias s='git status'
alias myissues='ghi list -s 'open'  -N "3 - Ready for Deploy" -N "5 - Ready for Client Review" -N "4 - Ready for Verification"  -N "0 - Collecting Information" -u "jimmyhmiller"'


run() {
    number=$1
    shift
    for i in `seq $number`; do
      $@
    done
}



antigen use oh-my-zsh
antigen bundle zsh-users/zsh-syntax-highlighting
# antigen theme pygmalion
antigen bundle mafredri/zsh-async
antigen bundle sindresorhus/pure
antigen apply

[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh


ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}⚡%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""


# PROMPT='%{$fg[yellow]%}Mac Pro%{$reset_color%}%{$fg[red]%}:%{$reset_color%}%{$fg[cyan]%}%0~%{$reset_color%}%{$fg[red]%}|%{$reset_color%}$(git_prompt_info)%{$fg[cyan]%}⇒%{$reset_color%}  '
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

# OPAM configuration
. /Users/jimmyhmiller/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

