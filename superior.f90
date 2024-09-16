program sistema_triangular_superior
    implicit none
    integer :: i, j, n
    real(8), allocatable :: A(:,:), b(:), x(:)
  
    ! Definir o tamanho do sistema
    print *, "Entre com o tamanho do sistema (n):"
    read(*,*) n
  
    ! Alocar memória para a matriz e os vetores
    allocate(A(n,n), b(n), x(n))
  
    ! Ler a matriz A (triangular superior)
    print *, "Entre com a matriz A (triangular superior):"
    do i = 1, n
       do j = 1, n
          if (j >= i) then
             read(*,*) A(i,j)
          else
             A(i,j) = 0.0 ! Definir elementos abaixo da diagonal como zero
          end if
       end do
    end do
  
    ! Ler o vetor b
    print *, "Entre com o vetor b:"
    do i = 1, n
       read(*,*) b(i)
    end do
  
    ! Resolver o sistema Ax = b (substituição retroativa)
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
  
  end program sistema_triangular_superior
  