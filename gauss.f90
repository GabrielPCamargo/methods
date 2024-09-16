program eliminacao_sem_pivotamento
    implicit none
    integer :: i, j, k, n
    real(8), allocatable :: A(:,:), b(:), x(:), m
  
    ! Definir o tamanho do sistema
    print *, "Entre com o tamanho do sistema (n):"
    read(*,*) n
  
    ! Alocar memória para a matriz A e os vetores b e x
    allocate(A(n,n), b(n), x(n))
  
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
  
    ! Etapa de eliminação de Gauss (sem pivotamento)
    do k = 1, n-1
       do i = k+1, n
          m = A(i,k) / A(k,k)
          A(i,k) = 0.0
          do j = k+1, n
             A(i,j) = A(i,j) - m * A(k,j)
          end do
          b(i) = b(i) - m * b(k)
       end do
    end do
  
    ! Substituição retroativa para encontrar o vetor solução x
    x(n) = b(n) / A(n,n)
    do i = n-1, 1, -1
       x(i) = b(i)
       do j = i+1, n
          x(i) = x(i) - A(i,j) * x(j)
       end do
       x(i) = x(i) / A(i,i)
    end do
  
    ! Exibir a solução
    print *, "A solução do sistema é:"
    do i = 1, n
       print *, "x(", i, ") = ", x(i)
    end do
  
    ! Liberar a memória alocada
    deallocate(A, b, x)
  
  end program eliminacao_sem_pivotamento
  