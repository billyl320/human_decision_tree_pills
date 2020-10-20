#data preprocessing
from keras.utils import to_categorical

#building the model
from keras.models import Sequential
from keras.layers import Dense, Conv2D, Flatten, MaxPooling2D


#my imported packages
import numpy as np#typical package (Kinser 2018)
import itertools as it #I don't think that this is used
import scipy.misc as sm #typical image package (Kinser 2018)
import convert as cvt #custom functions
import rpy2.robjects as robjects #interface with R
from keras.callbacks import EarlyStopping
from keras.regularizers import l2
from keras.layers import SpatialDropout2D

#importing for visualizing the CNN
import matplotlib
from matplotlib import pyplot as plt
import matplotlib.image as mpimg
import keras.models as km
from keras.preprocessing import image as kpi



dirs = ["triangle", "none"]
tri = cvt.GetAllImagesCNN(dirs)
dirs = ["square", "none"]
squs = cvt.GetAllImagesCNN(dirs)
dirs = ["pent", "reg_pent"]
pens = cvt.GetAllImagesCNN(dirs)
dirs = ["hex","reg_hex"]
hexs = cvt.GetAllImagesCNN(dirs)

#creating labels for each class
tri_labels  = [0]*len(tri)
squ_labels  = [1]*len(squs)
pent_labels = [2]*len(pens)
hex_labels  = [3]*len(hexs)

##################################
## training sample size = 3
##################################

#sample size
n=3
#initialize arrays to hold results
train_vals=np.zeros(100)
test_vals=np.zeros(100)

#splitting data into training and testing
rans = robjects.r("""
set.seed(83150)
n=3
#training datasets
train3<-matrix(nrow=100, ncol=n)
train4<-matrix(nrow=100, ncol=n)
train5<-matrix(nrow=100, ncol=n)
train6<-matrix(nrow=100, ncol=n)
#testing training sets
test3<-matrix(nrow=100, ncol=12-n)
test4<-matrix(nrow=100, ncol=8-n)
test5<-matrix(nrow=100, ncol=12-n)
test6<-matrix(nrow=100, ncol=8-n)
for(i in 1:100){
    train3[i,]<-sample(0:11,  n)
    temp<-0:11
    test3[i,]<-temp[-(train3[i,]+1)]
    train4[i,]<-sample(0:7, n)
    temp<-0:7
    test4[i,]<-temp[-(train4[i,]+1)]
    train5[i,]<-sample(0:11, n)
    temp<-0:11
    test5[i,]<-temp[-(train5[i,]+1)]
    train6[i,]<-sample(0:7, n)
    temp<-0:7
    test6[i,]<-temp[-(train6[i,]+1)]
}
""")
tri_ran = np.asarray(robjects.r["train3"])
squ_ran = np.asarray(robjects.r["train4"])
pent_ran = np.asarray(robjects.r["train5"])
hex_ran = np.asarray(robjects.r["train6"])

tri_test = np.asarray(robjects.r["test3"])
squ_test = np.asarray(robjects.r["test4"])
pent_test = np.asarray(robjects.r["test5"])
hex_test = np.asarray(robjects.r["test6"])



j=0
#############
#all classes
#############
#training
#training y labels
tri_y_train =  [tri_labels[i] for i in tri_ran[j]]
squ_y_train =  [squ_labels[i] for i in squ_ran[j]]
pent_y_train =  [pent_labels[i] for i in pent_ran[j]]
hex_y_train =  [hex_labels[i] for i in hex_ran[j]]
y_train = np.concatenate( (tri_y_train, squ_y_train, pent_y_train, hex_y_train), axis=0 )
#training images
tri_x_train =  [tri[i] for i in tri_ran[j]]
squ_x_train =  [squs[i] for i in squ_ran[j]]
pent_x_train =  [pens[i] for i in pent_ran[j]]
hex_x_train =  [hexs[i] for i in hex_ran[j]]
X_train = np.concatenate( (tri_x_train, squ_x_train, pent_x_train, hex_x_train), axis=0 )
#testing
#testing y labels
tri_y_test =  [tri_labels[i] for i in tri_test[j]]
squ_y_test =  [squ_labels[i] for i in squ_test[j]]
pent_y_test =  [pent_labels[i] for i in pent_test[j]]
hex_y_test =  [hex_labels[i] for i in hex_test[j]]
y_test = np.concatenate( (tri_y_test, squ_y_test, pent_y_test, hex_y_test), axis=0 )
#testing images
tri_x_test =  [tri[i] for i in tri_test[j]]
squ_x_test =  [squs[i] for i in squ_test[j]]
pent_x_test =  [pens[i] for i in pent_test[j]]
hex_x_test =  [hexs[i] for i in hex_test[j]]
X_test = np.concatenate( (tri_x_test, squ_x_test, pent_x_test, hex_x_test), axis=0 )
##################
#data pre-processing
##################
#reshape data to fit model
X_train = X_train.reshape(len(X_train),160,240,1)
X_test = X_test.reshape(len(X_test),160,240,1)
#convert
y_train = to_categorical(y_train)
y_test = to_categorical(y_test)
##################
#building the model
##################
#3 conv layers
#3 pooling layers
#with epochs with early stopping
#create model
model = Sequential()
#add early stopping component
ES = [EarlyStopping(monitor='loss', min_delta=0.001, patience=10)]
#add model layers
#first C layer
#Rectified Linear Unit activation, 64 nodes, 3x3 filter matrix, 160x240x1 images
model.add(Conv2D(64, kernel_size=3, activation='relu', input_shape=(160, 240, 1), activity_regularizer=l2(0.001) ))
#first pooling layer
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
#first spatial dropout
model.add(SpatialDropout2D(0.2))
#second C layer
#Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
#second pooling layer
model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
#second spatial dropout
model.add(SpatialDropout2D(0.2))
#third C layer
#Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
#third pooling layer
model.add(MaxPooling2D(pool_size=(2, 2)))
#third spatial dropout
model.add(SpatialDropout2D(0.2))
#adding 'flatten' layer
model.add(Flatten())
#6 nodes, softmax activation
model.add(Dense(4, activation='softmax'))
##################
#compiling the model
##################
#lower score indicates better performance
#also provides accuracy measure
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
##################
#training the model
##################
#uses validation dat
#epoch =100  means that the entire dataset is passed forward and backward through NN 100 times
# see https://towardsdatascience.com/epoch-vs-iterations-vs-batch-size-4dfb9c7ce9c9
model.fit(X_train, y_train, epochs=100, callbacks=ES, verbose=0)
##################
#making predictions
##################
#obtaining testing accuracy
vals=model.predict(X_train)
vals_pred = np.argmax(vals, axis=1)
#checking with truth
vals_truth = np.argmax(y_train, axis=1)
train_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)
#predicting on testing
vals=model.predict(X_test)
vals_pred = np.argmax(vals, axis=1)
#checking with truth
vals_truth = np.argmax(y_test, axis=1)
test_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)


#begining of visualization

#loading sample image from triangle
fname='tri_ex.jpg'

img_tri = kpi.load_img(fname, target_size=(160,240))
img_tensor = kpi.img_to_array(img_tri)
img_tensor = np.expand_dims(img_tensor, axis=0)
img_tensor /= 255.
img_tensor = img_tensor[:,:,:,0].reshape(1,160,240,1)

#check to see that image was properly loaded
#plt.imshow(img_tensor[0])
#plt.show()

# Extracts the outputs of the top 12 layers
layer_outputs = [layer.output for layer in model.layers[:12]]

# Creates a model that will return these outputs, given the model input
activation_model = km.Model(inputs=model.input, outputs=layer_outputs)

# Returns a list of five Numpy arrays: one array per layer activation
activations = activation_model.predict(img_tensor)

#example for first convolution layer
#first_layer_activation = activations[0]
#print(first_layer_activation.shape)

#printing first layer of convolution
#plt.matshow(first_layer_activation[0, :, :, 4], cmap='viridis')
#plt.show()

#showing all layers and nodes
layer_names = []
for layer in model.layers[:9]:
    # Names of the layers, so you can have them as part of your plot
    layer_names.append(layer.name)


images_per_row = 10
​
for layer_name, layer_activation in zip(layer_names, activations): # Displays the feature maps
    n_features = layer_activation.shape[-1] # Number of features in the feature map
    size0 = layer_activation.shape[1] #The feature map has shape (1, size0, size1, n_features).
    size1 = layer_activation.shape[2] #The feature map has shape (1, size0, size1, n_features).
    n_cols = n_features // images_per_row # Tiles the activation channels in this matrix
    display_grid = np.zeros((size0 * n_cols, images_per_row * size1))
    for col in range(n_cols): # Tiles each filter into a big horizontal grid
        for row in range(images_per_row):
            channel_image = layer_activation[0,
                                             :, :,
                                             col * images_per_row + row]
            channel_image -= channel_image.mean() # Post-processes the feature to make it visually palatable
            channel_image /= channel_image.std()
            channel_image *= 64
            channel_image += 128
            channel_image = np.clip(channel_image, 0, 255).astype('uint8')
            display_grid[col * size0 : (col + 1) * size0, # Displays the grid
                         row * size1 : (row + 1) * size1] = channel_image
    scale0 = 1. / size0
    scale1 = 1. / size1
    plt.figure(figsize=(scale0 * display_grid.shape[1],
                        scale1 * display_grid.shape[0]))
    plt.title('')
    plt.grid(False)
    plt.imshow(display_grid, aspect='auto', cmap='viridis')
    plt.axis('off')
    plt.savefig('plots/tri_'+layer_name+'.png')

#now with square
#loading sample image from triangle
fname='squ_ex.jpg'

img_tri = kpi.load_img(fname, target_size=(160,240))
img_tensor = kpi.img_to_array(img_tri)
img_tensor = np.expand_dims(img_tensor, axis=0)
img_tensor /= 255.
img_tensor = img_tensor[:,:,:,0].reshape(1,160,240,1)


# Extracts the outputs of the top 12 layers
layer_outputs = [layer.output for layer in model.layers[:12]]

# Creates a model that will return these outputs, given the model input
activation_model = km.Model(inputs=model.input, outputs=layer_outputs)

# Returns a list of five Numpy arrays: one array per layer activation
activations = activation_model.predict(img_tensor)

#showing all layers and nodes
layer_names = []
for layer in model.layers[:9]:
    # Names of the layers, so you can have them as part of your plot
    layer_names.append(layer.name)


images_per_row = 10
​
for layer_name, layer_activation in zip(layer_names, activations): # Displays the feature maps
    n_features = layer_activation.shape[-1] # Number of features in the feature map
    size0 = layer_activation.shape[1] #The feature map has shape (1, size0, size1, n_features).
    size1 = layer_activation.shape[2] #The feature map has shape (1, size0, size1, n_features).
    n_cols = n_features // images_per_row # Tiles the activation channels in this matrix
    display_grid = np.zeros((size0 * n_cols, images_per_row * size1))
    for col in range(n_cols): # Tiles each filter into a big horizontal grid
        for row in range(images_per_row):
            channel_image = layer_activation[0,
                                             :, :,
                                             col * images_per_row + row]
            channel_image -= channel_image.mean() # Post-processes the feature to make it visually palatable
            channel_image /= channel_image.std()
            channel_image *= 64
            channel_image += 128
            channel_image = np.clip(channel_image, 0, 255).astype('uint8')
            display_grid[col * size0 : (col + 1) * size0, # Displays the grid
                         row * size1 : (row + 1) * size1] = channel_image
    scale0 = 1. / size0
    scale1 = 1. / size1
    plt.figure(figsize=(scale0 * display_grid.shape[1],
                        scale1 * display_grid.shape[0]))
    plt.title('')
    plt.grid(False)
    plt.imshow(display_grid, aspect='auto', cmap='viridis')
    plt.axis('off')
    plt.savefig('plots/squ_'+layer_name+'.png')



#now with pentagon
#loading sample image from triangle
fname='pen_ex.jpg'

img_tri = kpi.load_img(fname, target_size=(160,240))
img_tensor = kpi.img_to_array(img_tri)
img_tensor = np.expand_dims(img_tensor, axis=0)
img_tensor /= 255.
img_tensor = img_tensor[:,:,:,0].reshape(1,160,240,1)


# Extracts the outputs of the top 12 layers
layer_outputs = [layer.output for layer in model.layers[:12]]

# Creates a model that will return these outputs, given the model input
activation_model = km.Model(inputs=model.input, outputs=layer_outputs)

# Returns a list of five Numpy arrays: one array per layer activation
activations = activation_model.predict(img_tensor)

#showing all layers and nodes
layer_names = []
for layer in model.layers[:9]:
    # Names of the layers, so you can have them as part of your plot
    layer_names.append(layer.name)


images_per_row = 10
​
for layer_name, layer_activation in zip(layer_names, activations): # Displays the feature maps
    n_features = layer_activation.shape[-1] # Number of features in the feature map
    size0 = layer_activation.shape[1] #The feature map has shape (1, size0, size1, n_features).
    size1 = layer_activation.shape[2] #The feature map has shape (1, size0, size1, n_features).
    n_cols = n_features // images_per_row # Tiles the activation channels in this matrix
    display_grid = np.zeros((size0 * n_cols, images_per_row * size1))
    for col in range(n_cols): # Tiles each filter into a big horizontal grid
        for row in range(images_per_row):
            channel_image = layer_activation[0,
                                             :, :,
                                             col * images_per_row + row]
            channel_image -= channel_image.mean() # Post-processes the feature to make it visually palatable
            channel_image /= channel_image.std()
            channel_image *= 64
            channel_image += 128
            channel_image = np.clip(channel_image, 0, 255).astype('uint8')
            display_grid[col * size0 : (col + 1) * size0, # Displays the grid
                         row * size1 : (row + 1) * size1] = channel_image
    scale0 = 1. / size0
    scale1 = 1. / size1
    plt.figure(figsize=(scale0 * display_grid.shape[1],
                        scale1 * display_grid.shape[0]))
    plt.title('')
    plt.grid(False)
    plt.imshow(display_grid, aspect='auto', cmap='viridis')
    plt.axis('off')
    plt.savefig('plots/pen_'+layer_name+'.png')




#now with hexagon
#loading sample image from triangle
fname='hex_ex.jpg'

img_tri = kpi.load_img(fname, target_size=(160,240))
img_tensor = kpi.img_to_array(img_tri)
img_tensor = np.expand_dims(img_tensor, axis=0)
img_tensor /= 255.
img_tensor = img_tensor[:,:,:,0].reshape(1,160,240,1)


# Extracts the outputs of the top 12 layers
layer_outputs = [layer.output for layer in model.layers[:12]]

# Creates a model that will return these outputs, given the model input
activation_model = km.Model(inputs=model.input, outputs=layer_outputs)

# Returns a list of five Numpy arrays: one array per layer activation
activations = activation_model.predict(img_tensor)

#showing all layers and nodes
layer_names = []
for layer in model.layers[:9]:
    # Names of the layers, so you can have them as part of your plot
    layer_names.append(layer.name)


images_per_row = 10
​
for layer_name, layer_activation in zip(layer_names, activations): # Displays the feature maps
    n_features = layer_activation.shape[-1] # Number of features in the feature map
    size0 = layer_activation.shape[1] #The feature map has shape (1, size0, size1, n_features).
    size1 = layer_activation.shape[2] #The feature map has shape (1, size0, size1, n_features).
    n_cols = n_features // images_per_row # Tiles the activation channels in this matrix
    display_grid = np.zeros((size0 * n_cols, images_per_row * size1))
    for col in range(n_cols): # Tiles each filter into a big horizontal grid
        for row in range(images_per_row):
            channel_image = layer_activation[0,
                                             :, :,
                                             col * images_per_row + row]
            channel_image -= channel_image.mean() # Post-processes the feature to make it visually palatable
            channel_image /= channel_image.std()
            channel_image *= 64
            channel_image += 128
            channel_image = np.clip(channel_image, 0, 255).astype('uint8')
            display_grid[col * size0 : (col + 1) * size0, # Displays the grid
                         row * size1 : (row + 1) * size1] = channel_image
    scale0 = 1. / size0
    scale1 = 1. / size1
    plt.figure(figsize=(scale0 * display_grid.shape[1],
                        scale1 * display_grid.shape[0]))
    plt.title('')
    plt.grid(False)
    plt.imshow(display_grid, aspect='auto', cmap='viridis')
    plt.axis('off')
    plt.savefig('plots/hex'+layer_name+'.png')
