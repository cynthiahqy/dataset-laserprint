library(magick)
library(tesseract)

pdf <- image_read_pdf("PrinterIndex/01-PrinterIndex-1987_cleaned.pdf")
print(pdf)

pdf2ocr <- image_ocr(pdf)

cat(pdf2ocr[1])