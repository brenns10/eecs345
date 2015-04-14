# Run all tests at once, just to make it more efficient.
CMD="(load \"test.scm\") "
for TEST in $(echo tests_* | sed "s/ /\n/g"); do
    CMD="$CMD (test \"$TEST\")"
done
echo "$CMD" | racket
