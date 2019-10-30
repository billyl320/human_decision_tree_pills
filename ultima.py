#converting images for analysis in R
#importing custom module for analysis
import convert as cvt

#desired directories
#note that each class should be separated into different directories.
#however, for the fucntion to work, multiple directories should be specified.
#thus, an empty folder is utilized for this task
#the empty folder is called "none"

#----------------------------------------------------------
#capsule class

caps = ["data/capsule", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'caps.txt'

#converting images
#cvt.BinaryHistTXT(name, caps, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+caps[0][5:]] )

#----------------------------------------------------------
#diamond class

diam = ["data/diamond", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'diam.txt'

#converting images
#cvt.BinaryHistTXT(name, diam, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+diam[0][5:]] )


#----------------------------------------------------------
#hexagon class

hex = ["data/hex", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'hex.txt'

#converting images
#cvt.BinaryHistTXT(name, hex, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+hex[0][5:]] )

hex = ["data/reg_hex", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'reg_hex.txt'

#converting images
#cvt.BinaryHistTXT(name, hex, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+hex[0][5:]] )


#----------------------------------------------------------
#oval class

oval = ["data/oval1", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'oval1.txt'

#converting images
#cvt.BinaryHistTXT(name, oval, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+oval[0][5:]] )

oval = ["data/oval2", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'oval2.txt'

#converting images
#cvt.BinaryHistTXT(name, oval, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+oval[0][5:]] )

oval = ["data/oval3", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'oval3.txt'

#converting images
#cvt.BinaryHistTXT(name, oval, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+oval[0][5:]] )

#----------------------------------------------------------
#pentagon class

pent = ["data/pent", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'pent.txt'

#converting images
#cvt.BinaryHistTXT(name, pent, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+pent[0][5:]] )

pent = ["data/reg_pent", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'reg_pent.txt'

#converting images
#cvt.BinaryHistTXT(name, pent, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+pent[0][5:]] )


#----------------------------------------------------------
#rectangle class

rect = ["data/rectangle", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'rect.txt'

#converting images
#cvt.BinaryHistTXT(name, rect, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+rect[0][5:]] )

#----------------------------------------------------------
#semi-circle class

sc = ["data/semi_circ", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'sc.txt'

#converting images
#cvt.BinaryHistTXT(name, sc, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+sc[0][5:]] )

#----------------------------------------------------------
#square class

squ = ["data/square", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'squ.txt'

#converting images
#cvt.BinaryHistTXT(name, squ, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+squ[0][5:]] )

#----------------------------------------------------------
#tear class

tear = ["data/tear", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'tear.txt'

#converting images
#cvt.BinaryHistTXT(name, tear, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+tear[0][5:]] )

#----------------------------------------------------------
#trapezoid class

trap = ["data/trap", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'trap.txt'

#converting images
#cvt.BinaryHistTXT(name, trap, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+trap[0][5:]] )

#----------------------------------------------------------
#triangle class

tri = ["data/triangle", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'tri.txt'

#converting images
#cvt.BinaryHistTXT(name, tri, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+tri[0][5:]] )


#----------------------------------------------------------
#round class

circ = ["data/circle1", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'round1.txt'

#converting images
#cvt.BinaryHistTXT(name, circ, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+circ[0][5:]] )

circ = ["data/circle2", "none"]

#name of .txt file
name = 'round2.txt'

#converting images
#cvt.BinaryHistTXT(name, circ, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+circ[0][5:]] )

circ = ["data/circle3", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'round3.txt'

#converting images
#cvt.BinaryHistTXT(name, circ, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+circ[0][5:]] )

circ = ["data/circle4", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'round4.txt'

#converting images
#cvt.BinaryHistTXT(name, circ, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+circ[0][5:]] )

circ = ["data/circle5", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'round5.txt'

#converting images
#cvt.BinaryHistTXT(name, circ, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+circ[0][5:]] )


#
