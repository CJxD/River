#!/bin/bash

declare -A repl
repl[true]="joolean"
repl[false]="notjoolean"

repl["if "]="pawel? "
repl[then]="pawel!"
repl[else]="julian!"
repl[endif]="sobocinski!"

repl[min]="lowbocinski"
repl[max]="highbocinski"
repl[average]="averacinski"
repl[round]="roundocinski"
repl[floor]="flobocinski"
repl[ceil]="ceilocinski"
repl[typeof]="julian?"

repl[debug]="prawel"

repl[contains]="has"
repl[append]="canhas"
repl[remove]="cannothas"
repl[length]="howmany"
repl[get]="plz"
repl[head]="head"
repl[tail]="toes"

repl[with]="julian"
repl[begin]="rathke"
repl[loop]="pawel"
repl[skip]="sobocinski"
repl["in "]="pawelhas "
repl["out "]="pawelcanhas "

while read line; do 
    echo $p
done < "spl/*.spl"

for i in "${!repl[@]}"
do
	sed -i "s/$i/${repl[$i]}/g" spl/*.spl
done
