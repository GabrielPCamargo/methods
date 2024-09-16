program fatoracao_LU
    implicit none
    integer :: n, i, j, k
    real(8), allocatable :: A(:,:), L(:,:), U(:,:), b(:), y(:), x(:)
  
    ! Ler o tamanho do sistema
    print *, "Entre com o tamanho do sistema (n):"
    read(*,*) n
  
    ! Alocar espaço para a matriz A, as matrizes L, U, o vetor b e as soluções y e x
    allocate(A(n,n), L(n,n), U(n,n), b(n), y(n), x(n))
  
    ! Inicializar L como a matriz identidade e U como a matriz zero
    L = 0.0
    U = 0.0
    do i = 1, n
       L(i,i) = 1.0
    end do
  
    ! Ler a matriz A
    print *, "Entre com a matriz A:"
    do i = 1, n
       do j = 1, n
          read(*,*) A(i,j)
       end do
    end do
  
    ! Ler o vetor b
    print *, "Entre com o vetor b:"
    do i = 1, n
       read(*,*) b(i)
    end do
  
    ! Fatoração LU de A
    do k = 1, n
       ! Calcular a matriz U (triangular superior)
       do j = k, n
          U(k,j) = A(k,j)
          do i = 1, k-1
             U(k,j) = U(k,j) - L(k,i) * U(i,j)
          end do
       end do
  
       ! Calcular a matriz L (triangular inferior)
       do i = k+1, n
          L(i,k) = A(i,k)
          do j = 1, k-1
             L(i,k) = L(i,k) - L(i,j) * U(j,k)
          end do
          L(i,k) = L(i,k) / U(k,k)
       end do
    end do
  
    ! Resolver o sistema Ly = b (substituição direta)
    do i = 1, n
       y(i) = b(i)
       do j = 1, i-1
          y(i) = y(i) - L(i,j) * y(j)
       end do
    end do
  
    ! Resolver o sistema Ux = y (substituição retroativa)
    do i = n, 1, -1
       x(i) = y(i)
       do j = i+1, n
          x(i) = x(i) - U(i,j) * x(j)
       end do
       x(i) = x(i) / U(i,i)
    end do
  
    ! Imprimir a solução x
    print *, "A solução do sistema é:"
    do i = 1, n
       print *, "x(", i, ") = ", x(i)
    end do
  
    ! Liberar a memória alocada
    deallocate(A, L, U, b, y, x)
  
  end program fatoracao_LU
  