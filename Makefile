PaletteExtractor:
	sbcl --script PaletteExtractor.lisp

PaletteGenerator:
	python PaletteGenerator.py

TransferModel:
	tensorflowjs_converter --input_format keras ./models/final.h5 ./website/static