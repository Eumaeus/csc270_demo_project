FILES=*.md
for f in $FILES
do
  filename="${f%.*}"
  echo "Converting $f to $filename.docx"
  `pandoc $f -o $filename.docx`
  # uncomment this line to delete the source file.
  # rm $f
done