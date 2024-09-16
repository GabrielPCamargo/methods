program eliminacao_com_pivotamento
    implicit none
    integer :: i, j, k, n, max_row
    real(8), allocatable :: A(:,:), b(:), x(:), m, temp
    real(8) :: max_val
  
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
  
    ! Eliminação de Gauss com pivotamento parcial
    do k = 1, n-1
       ! Encontrar a linha com o maior valor absoluto na coluna k
       max_row = k
       max_val = abs(A(k,k))
       do i = k+1, n
          if (abs(A(i,k)) > max_val) then
             max_val = abs(A(i,k))
             max_row = i
          end if
       end do
  
       ! Trocar as linhas se necessário
       if (max_row /= k) then
          do j = k, n
             temp = A(k,j)
             A(k,j) = A(max_row,j)
             A(max_row,j) = temp
          end do
          temp = b(k)
          b(k) = b(max_row)
          b(max_row) = temp
       end if
  
       ! Eliminação de Gauss
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
  
  end program eliminacao_com_pivotamento
  