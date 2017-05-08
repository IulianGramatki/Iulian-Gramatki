function A_inv_b = matrixInverseVector(A, b, x_init, alpha)
  x=x_init
  cost=norm(A*x-b)
 
  while cost>=0.001
    x=x-alpha*2*A*(A*x-b)
    cost=norm(A*x-b)
  end 
  
  A_inv_b=x
end