PANOPTS=-s \
	--filter pandoc-citeproc \
	--filter filters/pandocfilters/defenv.py \
	--filter filters/pandocfilters/linkref.py \
	--natbib \
	-N \
	-f markdown+raw_tex+tex_math_dollars+citations \
	--lua-filter=filters/lua/scholarly-metadata.lua \
	--lua-filter=filters/lua/author-info-blocks.lua \
	# --listings 
	# --filter filters/pandocfilters/listing.py \
	# --template=template.tex
INPUTDIR=sections/

weave:
	julia --project src/weave.jl


.PHONY: weave