function [J, grad] = linearRegCostFunction(X, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
%   cost of using theta as the parameter for linear regression to fit the 
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear 
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%

% X = [m 2], y = [m 1]
J = (1.0 / 2 / m) * sum((X*theta - y) .^2) + lambda/2.0/m * sum(theta(2:end).^2);

% gradient
grad = 1.0/m * (X' *(X*theta - y));
grad(2:end) = grad(2:end) + lambda * 1.0/m * theta(2:end);




% =========================================================================

grad = grad(:);

end
