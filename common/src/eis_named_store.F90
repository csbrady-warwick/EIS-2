MODULE eis_named_store_mod

  USE eis_ordered_store_mod
  IMPLICIT NONE

  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)
  INTEGER, PARAMETER :: REAL64 = SELECTED_REAL_KIND(15, 307)
  INTEGER(INT64), PARAMETER :: default_bucket_count = 100

  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE copy_named_store
  END INTERFACE

  !>Item stored in a named store
  TYPE :: named_store_item
    CHARACTER(LEN=:), ALLOCATABLE :: name
    LOGICAL :: owns = .TRUE.
    CLASS(*), POINTER :: item => NULL()
    CONTAINS
    PROCEDURE :: cleanup => nsi_cleanup
    FINAL :: nsi_destructor
  END TYPE named_store_item

  !> Simple list holding all items in a single bucket of the hash table
  TYPE :: named_store_inner_list
    TYPE(named_store_item), DIMENSION(:), POINTER :: list => NULL()
    CONTAINS
    PROCEDURE, PRIVATE :: get_index => nsil_get_index
    PROCEDURE :: unlink => nsil_unlink
    PROCEDURE :: get => nsil_get
    PROCEDURE :: store => nsil_store
    PROCEDURE :: delete => nsil_delete
    PROCEDURE :: copy => nsil_copy
    FINAL :: nsil_destructor
  END TYPE named_store_inner_list

  !> Associative array implemented using hash table
  TYPE :: named_store
    PRIVATE
    TYPE(named_store_inner_list), DIMENSION(:), ALLOCATABLE :: buckets
    TYPE(ordered_store) :: names
    INTEGER(INT64) :: count
    LOGICAL, PUBLIC :: is_init = .FALSE.
    LOGICAL, PUBLIC :: needs_optimise = .FALSE.
    CONTAINS
    PRIVATE
    PROCEDURE :: hash => ns_hash
    PROCEDURE :: store_name => ns_store_name
    PROCEDURE :: remove_name => ns_remove_name
    PROCEDURE :: get_by_name => ns_get_by_name
    PROCEDURE :: get_by_number => ns_get_by_number
    PROCEDURE, PUBLIC :: store => ns_store
    PROCEDURE, PUBLIC :: hold => ns_hold
    PROCEDURE, PUBLIC :: unlink => ns_unlink
    PROCEDURE, PUBLIC :: optimise => ns_optimise
    PROCEDURE, PUBLIC :: optimize => ns_optimise
    PROCEDURE, PUBLIC :: delete => ns_delete
    PROCEDURE, PUBLIC :: get_name_count => ns_get_name_count
    PROCEDURE, PUBLIC :: get_count => ns_get_name_count
    PROCEDURE, PUBLIC :: get_name => ns_get_name
    PROCEDURE :: rebucket => ns_rebucket
    PROCEDURE :: init_i8 => ns_init_i8
    PROCEDURE :: init_i4 => ns_init_i4
    PROCEDURE, PUBLIC :: cleanup => ns_cleanup
    PROCEDURE, PUBLIC :: copy => ns_copy
    GENERIC, PUBLIC :: init => init_i8, init_i4
    GENERIC, PUBLIC :: get => get_by_name, get_by_number
    FINAL :: ns_destructor
  END TYPE named_store

  PRIVATE
  PUBLIC :: named_store

CONTAINS


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocates the item held in the store
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE nsi_cleanup(this)
    CLASS(named_store_item), INTENT(INOUT) :: this !< self pointer

    IF (ASSOCIATED(this%item) .AND. this%owns) DEALLOCATE(this%item)
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
  PURE ELEMENTAL SUBROUTINE nsi_destructor(this)
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
  !> Unlink all of the items in the list. Needed for copying
  !> @param[in] this
  SUBROUTINE nsil_unlink(this)

    CLASS(named_store_inner_list), INTENT(IN) :: this !< self pointer

    IF (ASSOCIATED(this%list)) THEN
      CALL unlink_items(this%list)
    END IF

  END SUBROUTINE nsil_unlink



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
  !> Add an item to the inner list. Does not take a copy of item. Returns
  !> .TRUE. if the item name is new in the list
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  !> @param[in] owns
  !> @return nsil_store
  FUNCTION nsil_store(this, name, item, owns)

    CLASS(named_store_inner_list), INTENT(INOUT) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name to add item under
    CLASS(*), POINTER, INTENT(IN) :: item !< Item to add
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    LOGICAL :: nsil_store
    TYPE(named_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER :: sz, iindex

    iindex = this%get_index(name)

    IF (iindex > 0) THEN
      CALL this%list(iindex)%cleanup()
      ALLOCATE(this%list(iindex)%name, SOURCE = name)
      this%list(iindex)%item => item
      this%list(iindex)%owns = .TRUE.
      IF (PRESENT(owns)) this%list(iindex)%owns = owns
      nsil_store = .FALSE.      
      RETURN
    END IF

    nsil_store = .TRUE.

    IF (ASSOCIATED(this%list)) THEN
      sz = SIZE(this%list)
      ALLOCATE(temp(sz), SOURCE = this%list)
      CALL this%unlink()
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
    this%list(sz)%owns = .TRUE.
    IF (PRESENT(owns)) this%list(sz)%owns = owns
    this%list(sz)%item => item

  END FUNCTION nsil_store


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Delete an item from the list
  !> @param[in] this
  !> @param[in] name
  !> @return nsil_delete
  FUNCTION nsil_delete(this, name)
    CLASS(named_store_inner_list), INTENT(INOUT) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< Name of the item to delete
    LOGICAL :: nsil_delete
    INTEGER :: iindex
    TYPE(named_store_item), DIMENSION(:), POINTER :: list
    nsil_delete = .FALSE.
    IF (.NOT. ASSOCIATED(this%list)) RETURN
    iindex = this%get_index(name)
    IF (iindex > 0) THEN
      nsil_delete = .TRUE.
      ALLOCATE(list(SIZE(this%list)-1))
      IF (iindex > 1) THEN
        list(1:iindex-1) = this%list(1:iindex-1)
      END IF
      IF (iindex < SIZE(this%list)) THEN
        list(iindex:) = this%list(iindex+1:)
      END IF
      CALL this%unlink()
      DEALLOCATE(this%list)
      this%list => list
    END IF

  END FUNCTION nsil_delete



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Copy a list to another list
  !> @param[in] this
  !> @param[in] dest
  !> @param[in] shallow
  !> @return nsil_delete
  SUBROUTINE nsil_copy(this, dest, shallow)
    CLASS(named_store_inner_list), INTENT(IN) :: this !< self pointer
    CLASS(named_store_inner_list), INTENT(INOUT) :: dest !< Destination
    LOGICAL, INTENT(IN), OPTIONAL :: shallow
    LOGICAL :: do_shallow
    INTEGER(INT64) :: i

    do_shallow = .FALSE.
    IF (PRESENT(shallow)) do_shallow = shallow
    ALLOCATE(dest%list(SIZE(this%list, KIND=INT64)))
    DO i = 1, SIZE(this%list, KIND=INT64)
      ALLOCATE(dest%list(i)%name, SOURCE = this%list(i)%name)
      IF (this%list(i)%owns .AND. .NOT. do_shallow) THEN
        ALLOCATE(dest%list(i)%item, SOURCE = this%list(i)%item)
        dest%list(i)%owns = .TRUE.
      ELSE
        dest%list(i)%item => this%list(i)%item
        dest%list(i)%owns = .FALSE.
      END IF
    END DO

  END SUBROUTINE nsil_copy



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocate a list
  !> @param[in] this
  PURE ELEMENTAL SUBROUTINE nsil_destructor(this)

    TYPE(named_store_inner_list), INTENT(INOUT) :: this !< self pointer

    IF (.NOT. ASSOCIATED(this%list)) RETURN
    DEALLOCATE(this%list)
    this%list => NULL()

  END SUBROUTINE nsil_destructor



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Function to generate a hash from a name
  !> Implementation of djb2 hash
  !> @param[in] this
  !> @param[in] name
  !> @param[in] bucket_count
  !> @return hash
  FUNCTION ns_hash(this, name, bucket_count) RESULT(hash)

    CLASS(named_store), INTENT(IN) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< name to look up
    !> Optional number of buckets to use when calculating the hash lookup
    !> If not present then uses the number of buckets currently allocated
    INTEGER(INT64), INTENT(IN), OPTIONAL :: bucket_count

    INTEGER(INT64) :: hash, nb, i

    hash = 5381
    IF (PRESENT(bucket_count)) THEN
      nb = bucket_count
    ELSE
      nb = SIZE(this%buckets, KIND=INT64)
    END IF
    DO i = 1, LEN_TRIM(name)
      hash = MOD((ISHFT(hash, 5) + hash) + INT(IACHAR(name(i:i)), INT64), nb)
    END DO
    hash = hash + 1 !Fortran 1 based array

  END FUNCTION ns_hash



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Store a name into the name store
  !> @param[in] this
  !> @param[in] name
  SUBROUTINE ns_store_name(this, name)

    CLASS(named_store), INTENT(INOUT) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< name to store
    INTEGER :: id

    id = this%names%store(name)

  END SUBROUTINE ns_store_name



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Remove a name in the name store
  !> @param[in] this
  !> @param[in] name
  SUBROUTINE ns_remove_name(this, name)

    CLASS(named_store), INTENT(INOUT) :: this !< self pointer
    CHARACTER(LEN=*), INTENT(IN) :: name !< name to remove
    INTEGER :: i
    CHARACTER(LEN=:), POINTER :: cptr
    CLASS(*), POINTER :: gptr
    LOGICAL :: success

    CALL this%names%enable_disorder()
    DO i = 1, this%names%get_size()
      gptr => this%names%get(i)
      SELECT TYPE (gptr)
        TYPE IS (CHARACTER(LEN=*))
          cptr => gptr
        CLASS DEFAULT
          cptr => NULL()
      END SELECT
      IF (ASSOCIATED(cptr)) THEN
        IF (cptr == name) THEN
          success = this%names%delete(i)
          EXIT
        END IF
      END IF
    END DO
    CALL this%names%enable_disorder()

  END SUBROUTINE ns_remove_name



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from the store by name
  !> return NULL pointer if item is not present
  !> @param[in] this
  !> @param[in] name
  !> @return item
  FUNCTION ns_get_by_name(this, name) RESULT (item)

    CLASS(named_store), INTENT(IN) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), POINTER :: item
    INTEGER(INT64) :: bucket

    item => NULL()
    IF (.NOT. ALLOCATED(this%buckets)) RETURN

    bucket = this%hash(name)
    item => this%buckets(bucket)%get(name)

  END FUNCTION ns_get_by_name



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from the store by name number
  !> return NULL pointer if number is out of range
  !> @param[in] this
  !> @param[in] index
  !> @return item
  FUNCTION ns_get_by_number(this, index) RESULT (item)

    CLASS(named_store), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index
    CLASS(*), POINTER :: item
    CHARACTER(LEN=:), ALLOCATABLE :: name

    item => NULL()
    IF (.NOT. ALLOCATED(this%buckets)) RETURN
    IF (index < 1 .OR. index > this%get_name_count()) RETURN
    CALL this%get_name(index, name)
    item => this%get_by_name(name)
    DEALLOCATE(name)

  END FUNCTION ns_get_by_number



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the hash table taking a copy
  !> when doing so
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  SUBROUTINE ns_store(this, name, item)

    CLASS(named_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), INTENT(IN) :: item
    CLASS(*), POINTER :: copy

    ALLOCATE(copy, SOURCE = item)
    CALL this%hold(name, copy, owns = .TRUE.)

  END SUBROUTINE ns_store



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the hash table storing the passed item NOT a copy.
  !> By default is set to assume that it owns the item after adding
  !> and deletes it when the store is closed. Set "owns = .FALSE." if
  !> you have other pointers to the item
  !> @param[in] this
  !> @param[in] name
  !> @param[in] item
  !> @param[in] owns
  SUBROUTINE ns_hold(this, name, item, owns)

    CLASS(named_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    CLASS(*), POINTER, INTENT(IN) :: item
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    INTEGER(INT64) :: bucket

    IF (.NOT. ALLOCATED(this%buckets)) &
        CALL this%init(default_bucket_count)

    this%needs_optimise = .TRUE.

    bucket = this%hash(name)
    IF (this%buckets(bucket)%store(name, item, owns)) CALL this%store_name(name)
    this%count = this%count + 1

  END SUBROUTINE ns_hold



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Delete an item specified by name
  !> @param[in] this
  !> @param[in] name
  SUBROUTINE ns_delete(this, name)

    CLASS(named_store), INTENT(INOUT) :: this
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(INT64) :: bucket

    IF (.NOT. ALLOCATED(this%buckets)) RETURN

    bucket = this%hash(name)
    IF (this%buckets(bucket)%delete(name)) THEN
      this%count = this%count - 1
      CALL this%remove_name(name)
    END IF

  END SUBROUTINE ns_delete



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the count of names
  !> @param[in] this
  !> @return ns_get_name_count
  FUNCTION ns_get_name_count(this)

    CLASS(named_store), INTENT(INOUT) :: this
    INTEGER :: ns_get_name_count

    ns_get_name_count = this%names%get_size()

  END FUNCTION ns_get_name_count



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the count of names
  !> @param[in] this
  !> @param[in] index
  !> @param[out] name
  SUBROUTINE ns_get_name(this, index, name)

    CLASS(named_store), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: name
    CLASS(*), POINTER :: gptr
    CHARACTER(LEN=:), POINTER :: cptr

    gptr => this%names%get(index)
    SELECT TYPE (gptr)
      TYPE IS (CHARACTER(LEN=*))
        cptr => gptr
      CLASS DEFAULT
        cptr => NULL()
    END SELECT

    IF (ASSOCIATED(cptr)) THEN
      ALLOCATE(name, SOURCE = cptr)  
    END IF

  END SUBROUTINE ns_get_name



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Unlink all of the items in all of the buckets
  !> needed when copying the list
  !> @param[in] this
  SUBROUTINE ns_unlink(this)

    CLASS(named_store), INTENT(IN) :: this !< self pointer
    INTEGER :: i

    IF (.NOT. ALLOCATED(this%buckets)) RETURN
    DO i = 1, SIZE(this%buckets)
      CALL this%buckets(i)%unlink()
    END DO

  END SUBROUTINE ns_unlink



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Optimise the performance of the hash table by optimising the number
  !> of buckets to match the number of stored items
  !> @param[in] this
  !> @param[in] item_bucket_fact
  !> @param[in] optimise_down
  SUBROUTINE ns_optimise(this, item_bucket_factor, optimise_down)

    CLASS(named_store), INTENT(INOUT) :: this !< self pointer
    !> Factor to apply to the number of items to calculate the number of
    !> buckets. Higher values reduce the likelihood of a hash collision
    !> (if the hashing algorithm is working well) but increases the memory
    !> foot print. Optional, default 1
    REAL(REAL64), INTENT(IN), OPTIONAL :: item_bucket_factor
    !> Whether to reduce the number of buckets below the current number
    !> if the number of items is smaller than the number of buckets. Optional,
    !> default .FALSE.
    LOGICAL, INTENT(IN), OPTIONAL :: optimise_down
    REAL(REAL64) :: mult
    LOGICAL :: od
    INTEGER(INT64) :: newcount

    IF (.NOT. this%needs_optimise .AND. .NOT. PRESENT(item_bucket_factor)) &
        RETURN

    mult = 1.0_REAL64
    IF (PRESENT(item_bucket_factor)) mult = item_bucket_factor

    od = .FALSE.
    IF (PRESENT(optimise_down)) od = optimise_down

    IF (.NOT. ALLOCATED(this%buckets)) RETURN
    IF (REAL(this%count, REAL64) * mult < SIZE(this%buckets) &
        .AND. .NOT. od) RETURN

    newcount = MAX(default_bucket_count, INT(REAL(this%count, REAL64) * mult, &
        INT64))

    CALL this%rebucket(newcount)

  END SUBROUTINE ns_optimise



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Routine to move the stored items into a new bucket system with
  !> a different number of buckets
  !> @param[inout] this
  !> @param[in] bucket_count
  SUBROUTINE ns_rebucket(this, bucket_count)
    CLASS(named_store), INTENT(INOUT) :: this !< self pointer
    INTEGER(INT64), INTENT(IN) :: bucket_count
    TYPE(named_store_inner_list), DIMENSION(:), ALLOCATABLE :: new_buckets
    INTEGER(INT64) :: ibucket, ilist, h
    TYPE(named_store_item), POINTER :: item
    LOGICAL :: is_new

    ALLOCATE(new_buckets(bucket_count))

    DO ibucket = 1, SIZE(this%buckets)
      IF (ASSOCIATED(this%buckets(ibucket)%list)) THEN
        DO ilist = 1, SIZE(this%buckets(ibucket)%list)
          item => this%buckets(ibucket)%list(ilist)
          h = this%hash(item%name, bucket_count)
          is_new = new_buckets(h)%store(item%name, item%item, owns = item%owns)
        END DO
        CALL this%buckets(ibucket)%unlink()
      END IF
    END DO
    DEALLOCATE(this%buckets)
    CALL MOVE_ALLOC(new_buckets, this%buckets)

  END SUBROUTINE ns_rebucket



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Initialise a hash table to a given size
  !> from an I8 integer
  !> @param[in] this
  !> @param[in] bucket_count
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
  !> @param[in] bucket_count
  SUBROUTINE ns_init_i4(this, bucket_count)

    CLASS(named_store), INTENT(INOUT) :: this
    INTEGER(INT32), INTENT(IN) :: bucket_count

    CALL this%init(INT(bucket_count,INT64))

  END SUBROUTINE ns_init_i4



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Copy a named store 
  !> @param[in] this
  !> @param[in] dest
  SUBROUTINE ns_copy(this, dest, shallow)

    CLASS(named_store), INTENT(IN) :: this
    CLASS(named_store), INTENT(INOUT) :: dest
    LOGICAL, INTENT(IN), OPTIONAL :: shallow
    INTEGER(INT64) :: i

    IF (.NOT. this%is_init) RETURN
    IF (dest%is_init) CALL dest%cleanup()
    CALL dest%init(SIZE(this%buckets, KIND=INT64))
    DO i = 1, SIZE(this%buckets, KIND=INT64)
      CALL this%buckets(i)%copy(dest%buckets(i), shallow)
    END DO
    dest%count = this%count
    dest%needs_optimise = this%needs_optimise

  END SUBROUTINE ns_copy



  !> Cleans up an item
  PURE ELEMENTAL SUBROUTINE ns_cleanup(this)

    CLASS(named_store), INTENT(INOUT) :: this

    IF (.NOT. ALLOCATED(this%buckets)) RETURN
    DEALLOCATE(this%buckets)
    this%is_init = .FALSE.

  END SUBROUTINE ns_cleanup



  !> Delete all inner lists on destruction
  PURE ELEMENTAL SUBROUTINE ns_destructor(this)

    TYPE(named_store), INTENT(INOUT) :: this

    CALL this%cleanup()

  END SUBROUTINE ns_destructor



  SUBROUTINE copy_named_store(dest, source)
    CLASS(named_store), INTENT(INOUT) :: dest
    CLASS(named_store), INTENT(IN) :: source

    CALL source%copy(dest, .FALSE.)

  END SUBROUTINE copy_named_store

END MODULE eis_named_store_mod
