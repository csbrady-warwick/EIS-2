MODULE eis_named_store_mod

  IMPLICIT NONE

  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)
  INTEGER(INT64), PARAMETER :: default_bucket_count = 100

  TYPE :: named_store_item
    CHARACTER(LEN=:), ALLOCATABLE :: name
    CLASS(*), POINTER :: item => NULL()
    CONTAINS
    PROCEDURE :: cleanup => nsi_cleanup
    FINAL :: nsi_destructor
  END TYPE named_store_item

  TYPE :: named_store_inner_list
    TYPE(named_store_item), DIMENSION(:), POINTER :: list => NULL()
    CONTAINS
    PROCEDURE, PRIVATE :: get_index => nsil_get_index
    PROCEDURE :: get => nsil_get
    PROCEDURE :: store => nsil_store
    FINAL :: nsil_destructor
  END TYPE named_store_inner_list

  TYPE :: named_store
    PRIVATE
    TYPE(named_store_inner_list), DIMENSION(:), ALLOCATABLE :: buckets
    INTEGER(INT64) :: count
    LOGICAL, PUBLIC :: is_init = .FALSE.
    CONTAINS
    PRIVATE
    PROCEDURE :: hash => ns_hash
    PROCEDURE, PUBLIC :: get => ns_get
    PROCEDURE, PUBLIC :: store => ns_store
    PROCEDURE :: init_i8 => ns_init_i8
    PROCEDURE :: init_i4 => ns_init_i4
    GENERIC, PUBLIC :: init => init_i8, init_i4
    FINAL :: ns_destructor
  END TYPE named_store

  PRIVATE
  PUBLIC :: named_store

CONTAINS


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocates the item held in the store
  !> @param[inout] this
  SUBROUTINE nsi_cleanup(this)
    CLASS(named_store_item), INTENT(INOUT) :: this !< self pointer

    IF (ASSOCIATED(this%item)) DEALLOCATE(this%item)
    IF (ALLOCATED(this%name)) DEALLOCATE(this%name)
    this%item => NULL()
  END SUBROUTINE nsi_cleanup



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Unlinks but does not deallocate the associated item
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE unlink_items(item_store)
    CLASS(named_store_item), INTENT(INOUT) :: item_store !< itemstore

    item_store%item => NULL()
  END SUBROUTINE unlink_items



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Destructor for the item store. Deallocates the item held in the store
  !> @param[inout] this
  SUBROUTINE nsi_destructor(this)
    TYPE(named_store_item), INTENT(INOUT) :: this !< self pointer

    CALL this%cleanup()
  END SUBROUTINE nsi_destructor



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the index of an item in the store. Linear search
  !> @param[in] this
  !> @param[in] name
  !> @return item
  FUNCTION nsil_get_index(this, name) RESULT(item)

    CLASS(named_store_inner_list), INTENT(IN) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to look up
    INTEGER :: ifn, sz, item

    item = 0
    IF (ASSOCIATED(this%list)) THEN
      sz = SIZE(this%list)
      DO ifn = 1, sz
        IF (TRIM(this%list(ifn)%name) == TRIM(name)) THEN
          item = ifn
          EXIT
        END IF
      END DO
    END IF

  END FUNCTION nsil_get_index



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from the store by name
  !> @param[in] this
  !> @param[in] name
  !> @return item
  FUNCTION nsil_get(this, name) RESULT(item)

    CLASS(named_store_inner_list), INTENT(IN) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to look up
    CLASS(*), POINTER :: item
    INTEGER :: iindex

    item => NULL()
    iindex = this%get_index(name)
    IF (iindex > 0) THEN
      item => this%list(iindex)%item
    END IF

  END FUNCTION nsil_get


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the inner list. Takes a copy of the item
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  SUBROUTINE nsil_store(this, name, item)

    CLASS(named_store_inner_list), INTENT(INOUT) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to add item under
    CLASS(*), INTENT(IN) :: item !< Item to add
    TYPE(named_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: sz, ssz, iindex

    iindex = this%get_index(name)

    IF (iindex > 0) THEN
      CALL this%list(iindex)%cleanup()
      ALLOCATE(this%list(iindex)%item, SOURCE = item)
      RETURN
    END IF

    IF (ASSOCIATED(this%list)) THEN
      sz = SIZE(this%list)
      ALLOCATE(temp(sz), SOURCE = this%list)
      CALL unlink_items(this%list)
      DEALLOCATE(this%list)
      ALLOCATE(this%list(1:sz+1))
      this%list(1:sz) = temp(1:sz)
      CALL unlink_items(temp)
      DEALLOCATE(temp)
      sz = sz + 1
    ELSE
      sz = 1
      ALLOCATE(this%list(1)) 
    END IF
    ALLOCATE(this%list(sz)%name, SOURCE = name)
    ALLOCATE(this%list(sz)%item, SOURCE = item)

  END SUBROUTINE nsil_store



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocate a list
  !> @param[in] this
  PURE ELEMENTAL SUBROUTINE nsil_destructor(this)

    TYPE(named_store_inner_list), INTENT(INOUT) :: this !< self pointer

    IF (.NOT. ASSOCIATED(this%list)) RETURN
    this%list => NULL()
    DEALLOCATE(this%list)

  END SUBROUTINE nsil_destructor



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to generate a hash from a name
  !> Implementation of djb2 hash modified for fixed number of bins and
  !> Fortran style 1 indexed arrays
  !> @param[in] this
  !> @param[in] name
  !> @result hash
  FUNCTION ns_hash(this, name) RESULT(hash)

    CLASS(named_store), INTENT(IN) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< name to look up
    INTEGER(INT64) :: hash, nb, i

    hash = 5381
    nb = SIZE(this%buckets, KIND=INT64)
    DO i = 1, LEN_TRIM(name)
      hash = MOD((ISHFT(hash, 5) + hash) + INT(IACHAR(name(i:i)), INT64), nb)
    END DO
    hash = hash + 1 !Fortran 1 based array

  END FUNCTION ns_hash



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from the store by name
  !> return NULL pointer if item is not present
  !> @param[in] this
  !> @param[in] name
  !> @result item
  FUNCTION ns_get(this, name) RESULT (item)

    CLASS(named_store), INTENT(IN) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), POINTER :: item
    INTEGER(INT64) :: bucket

    item => NULL()
    IF (.NOT. ALLOCATED(this%buckets)) RETURN

    bucket = this%hash(name)
    item => this%buckets(bucket)%get(name)

  END FUNCTION ns_get



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the hash table
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  SUBROUTINE ns_store(this, name, item)

    CLASS(named_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), INTENT(IN) :: item
    INTEGER(INT64) :: bucket

    IF (.NOT. ALLOCATED(this%buckets)) &
        CALL this%init(default_bucket_count)

    bucket = this%hash(name)
    CALL this%buckets(bucket)%store(name, item)
    this%count = this%count + 1

  END SUBROUTINE ns_store



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise a hash table to a given size
  !> from an I8 integer
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  SUBROUTINE ns_init_i8(this, bucket_count)

    CLASS(named_store), INTENT(INOUT) :: this
    INTEGER(INT64), INTENT(IN) :: bucket_count
    INTEGER(INT64) :: local_count

    local_count = MAX(bucket_count, default_bucket_count)
    this%is_init = .TRUE.

    IF (.NOT. ALLOCATED(this%buckets)) THEN
      this%count = 0
      ALLOCATE(this%buckets(local_count))
    END IF

  END SUBROUTINE ns_init_i8



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise a hash table to a given size
  !> from an I4 integer
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  SUBROUTINE ns_init_i4(this, bucket_count)

    CLASS(named_store), INTENT(INOUT) :: this
    INTEGER(INT32), INTENT(IN) :: bucket_count

    CALL this%init(INT(bucket_count,INT64))

  END SUBROUTINE ns_init_i4



  !> Delete all inner lists on destruction

  PURE ELEMENTAL SUBROUTINE ns_destructor(this)

    TYPE(named_store), INTENT(INOUT) :: this

    IF (.NOT. ALLOCATED(this%buckets)) RETURN
    DEALLOCATE(this%buckets)

  END SUBROUTINE ns_destructor

END MODULE eis_named_store_mod
