import tensorflow as tf
import numpy as np

# Example dataset: 4 samples with 2 features each (like height and weight)
X = np.array([[1, 2], [2, 3], [3, 4], [4, 5]])  # Input features
y = np.array([[0], [0], [1], [1]])  # Binary target labels

# Create a simple logistic regression model using TensorFlow
model = tf.keras.Sequential([
    tf.keras.layers.Dense(1, input_shape=(2,), activation='sigmoid')  # 2 input features, 1 output (0 or 1)
])

# Compile the model (set loss function and optimizer)
model.compile(optimizer='sgd', loss='binary_crossentropy', metrics=['accuracy'])

# Train the model
model.fit(X, y, epochs=100)  # Train for 100 epochs

# Test the model with new data
test_data = np.array([[5, 6]])
prediction = model.predict(test_data)
print('Prediction:', prediction)