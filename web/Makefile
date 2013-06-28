index = ../readme.md
releases = ../news.md
css = xmobar.css
images = xmobar-sawfish.png xmobar-xmonad.png
remote = community.haskell.org:/srv/projects/xmobar/
htmls = index.html releases.html

index.html: releases.html $(index) $(css)
	pandoc -f markdown -t html -c $(css) --toc -N -s \
               $(index) > index.html

releases.html: $(releases) $(css)
	pandoc -f markdown -t html -c $(css) -s \
               $(releases) > releases.html

upload-images:
	rsync -zav $(images) $(remote)

upload-pages: $(htmls)
	rsync -zav $(htmls) $(css) $(remote)

upload: upload-images upload-pages

clean:
	rm -f $(htmls)
