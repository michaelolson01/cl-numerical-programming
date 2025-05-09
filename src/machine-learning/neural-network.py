import numpy as np
import matplotlib.pyplot as plt

def init_params(layer_dims):
    np.random.seed(3)
    params = {}
    L = len(layer_dims)
    for l in range(1, L):
        params['W'+str(l)] = np.random.randn(layer_dims[l], layer_dims[l-1])*0.01
        params['b'+str(l)] = np.zeros((layer_dims[l], 1))
    return params

# Z (linear hypothesis) - Z = W*X + b
# W - weight matrix, b- bias vector, X- Input

def sigmoid(Z):
    # Again, numpy lets you do everything with a dot.
    # the dot function here is actually a scalar times a vector.
    A = 1/(1+np.exp(np.dot(-1, Z)))
    cache = (Z)

    return A, cache

def forward_prop(X, params):
    A = X # input to first layer i.e. training data
    caches = []
    # this is because params is a dictionary, and every entry adds one length.
    # and this algorithm uses 2 per connetion - Weights and Bias.
    L = len(params)//2
    for l in range(1, L+1):
        A_prev = A

        # Linear Hypothesis
        # Note: numpy lets you do everything with a dot product..
        #       this is actually a matrix * vector multiplication.
        Z = np.dot(params['W'+str(l)], A_prev) + params['b'+str(l)]

        # Storing the linear cache: current input, current weight, current bias
        linear_cache = (A_prev, params['W'+str(l)], params['b'+str(l)])

        # Applying sigmoid on linear hypothesis
        # returned activation_cache is just the Z value inputted...
        # And that is the linear_hypothesis
        A, activation_cache = sigmoid(Z)

        # storing the both linear and activation cache
        cache = (linear_cache, activation_cache)
        caches.append(cache)

    return A, caches

def sigmoid(Z):
    A = 1/(1+np.exp(np.dot(-1, Z)))

    return A

def cost_function(A, Y):
    m = Y.shape[1]

    # question here is, what is dot doing, log is the natural log function.
    cost = (-1/m)*(np.dot(np.log(A), Y.T) + np.dot(log(1-A), 1-Y.T))

    return cost

def one_layer_backward(dA, cache):
    linear_cache, activation_cache = cache

    Z = activation_cache
    dZ = dA*sigmoid(Z)*(1-sigmoid(Z)) # The derivative of the sigmoid function

    A_prev, W, b = linear_cache
    m = A_prev.shape[1]

    dW = (1/m)*np.dot(dZ, A_prev.T)
    db = (1/m)*np.sum(dZ, axis=1, keepdims=True)
    dA_prev = np.dot(W.T, dZ)

    return dA_prev, dW, db

def backprop(AL, Y, caches):
    grads = {}
    L = len(caches)
    m = AL.shape[1]
    Y = Y.reshape(AL.shape)

    dAL = -(np.divide(Y, AL) - np.divide(1-Y, 1-AL))

    current_cache = caches[L-1]
    grads['dA'+str(L-1)], grads['dW'+str(L-1)], grads['db'+str(L-1)] = one_layer_backward(dAL, current_cache)

    for l in reversed(range(L-1)):

        current_cache = caches[l]
        dA_prev_temp, dW_temp, db_temp = one_layer_backward(grads["dA" + str(l+1)], current_cache)
        grads["dA" + str(l)] = dA_prev_temp
        grads["dW" + str(l + 1)] = dW_temp
        grads["db" + str(l + 1)] = db_temp

    return grads

def update_parameters(parameters, grads, learning_rate):
    L = len(parameters) // 2

    for l in range(L):
        parameters['W'+str(l+1)] = parameters['W'+str(l+1)] - learning_rate*grads['W'+str(l+1)]
        parameters['b'+str(l+1)] = parameters['b'+str(l+1)] - learning_rate*grads['b'+str(l+1)]

    return parameters

def train(X, Y, layer_dims, epochs, lr):
    params = init_params(layer_dims)
    cost_history = []

    for i in range(epochs):
        Y_hat, caches = forward_prop(X, params)
        cost = cost_function(Y_hat, Y)
        cost_history.append(cost)
        grads = backprop(Y_hat, Y, caches)

        params = update_parameters(params, grads, lr)

    return params, cost_history
