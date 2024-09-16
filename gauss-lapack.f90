program eliminacao_sem_pivotamento_lapack
    implicit none
    integer :: n, info, lda, ldb, i, j
    integer, allocatable :: ipiv(:)
    real(8), allocatable :: A(:,:), b(:)
  
    ! Declarar as rotinas LAPACK dgetrf e dgetrs
    external :: dgetrf, dgetrs
  
    ! Definir o tamanho do sistema
    print *, "Entre com o tamanho do sistema (n):"
    read(*,*) n
  
    lda = n
    ldb = n
  
    ! Alocar memória para a matriz A, vetor b e o vetor de pivotamento
    allocate(A(n,n), b(n), ipiv(n))
  
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
  
    ! Chamar a rotina LAPACK dgetrf para fazer a fatoração LU
    call dgetrf(n, n, A, lda, ipiv, info)
  
    ! Verificar se a fatoração foi bem-sucedida
    if (info == 0) then
       ! Chamar a rotina LAPACK dgetrs para resolver o sistema
       call dgetrs('N', n, 1, A, lda, ipiv, b, ldb, info)
  
       ! Verificar se a solução foi bem-sucedida
       if (info == 0) then
          print *, "A solução do sistema é:"
          do i = 1, n
             print *, "x(", i, ") = ", b(i)
          end do
       else
          print *, "Erro: A solução falhou com código", info
       end if
    else
       print *, "Erro: A fatoração LU falhou com código", info
    end if
  
    ! Liberar a memória alocada
    deallocate(A, b, ipiv)
  
  end program eliminacao_sem_pivotamento_lapack
  