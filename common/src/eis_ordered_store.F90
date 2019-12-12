MODULE eis_ordered_store_mod

  IMPLICIT NONE

  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)

  TYPE :: ordered_store_item
    CLASS(*), POINTER :: item => NULL()
    LOGICAL :: owns_item = .TRUE.
    CONTAINS
    PROCEDURE :: cleanup => osi_cleanup
    FINAL :: osi_destructor
  END TYPE ordered_store_item

  TYPE :: ordered_store
    PRIVATE
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: items
    LOGICAL, PUBLIC :: is_init = .FALSE.
    INTEGER :: disorder_level = 0
    CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC :: get_size => os_get_size
    PROCEDURE, PUBLIC :: get => os_get
    PROCEDURE :: store_core => os_store_core
    PROCEDURE, PUBLIC :: store => os_store
    PROCEDURE, PUBLIC :: hold => os_hold
    PROCEDURE, PUBLIC :: delete => os_delete
    PROCEDURE, PUBLIC :: insert => os_insert
    PROCEDURE, PUBLIC :: clear => os_clear
    PROCEDURE, PUBLIC :: enable_disorder => os_enable_disorder
    PROCEDURE, PUBLIC :: disable_disorder => os_disable_disorder
    FINAL :: os_destructor
  END TYPE ordered_store

  PRIVATE
  PUBLIC :: ordered_store

CONTAINS


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocates the item held in the store
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE osi_cleanup(this)
    CLASS(ordered_store_item), INTENT(INOUT) :: this !< self pointer

    IF (ASSOCIATED(this%item) .AND. this%owns_item) DEALLOCATE(this%item)
    this%item => NULL()
  END SUBROUTINE osi_cleanup



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Unlinks but does not deallocate the associated item
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE unlink_items(item_store)
    CLASS(ordered_store_item), INTENT(INOUT) :: item_store !< itemstore

    item_store%item => NULL()
  END SUBROUTINE unlink_items



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Destructor for the item store. Deallocates the item held in the store
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE osi_destructor(this)
    TYPE(ordered_store_item), INTENT(INOUT) :: this !< self pointer

    CALL this%cleanup()
  END SUBROUTINE osi_destructor



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of items in this store. Returns 0 if empty
  !> @param[in] this
  !> @return item
  FUNCTION os_get_size(this) RESULT (sz)

    CLASS(ordered_store), INTENT(IN) :: this
    INTEGER :: sz !<Number of items in the store

    sz = 0
    IF (.NOT. ALLOCATED(this%items)) RETURN
    sz = SIZE(this%items)

  END FUNCTION os_get_size



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from the store by index
  !> return NULL pointer if item is not present
  !> @param[in] this
  !> @param[in] index
  !> @return item
  FUNCTION os_get(this, index) RESULT (item)

    CLASS(ordered_store), INTENT(IN) :: this
    INTEGER(INT32), INTENT(IN) :: index !< Index of the item to get
    CLASS(*), POINTER :: item !< Returned item

    item => NULL()
    IF (.NOT. ALLOCATED(this%items)) RETURN
    IF (index < 1 .OR. index > SIZE(this%items)) RETURN

    item => this%items(index)%item

  END FUNCTION os_get



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the ordered store
  !> @param[in] this
  !> @param[in] item
  !> @param[in] owns
  !> @return os_store
  FUNCTION os_store_core(this, item, index, owns) RESULT(os_store)

    CLASS(ordered_store), INTENT(INOUT) :: this
    CLASS(*), POINTER, INTENT(IN) :: item !< Item so store
    !> Index to store the item at. Intended to overwrite an existing item
    !> Not to allow storage in an arbitrary location. If index is not an 
    !> existing item index then the item is simply added to the end of the list
    INTEGER, INTENT(IN), OPTIONAL :: index
    !> Whether the store owns the pointer to the item or not. Optional,
    !> default true
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    INTEGER(INT32) :: os_store !< Index to which the item is stored
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER(INT32) :: sz
    LOGICAL :: should_own

    should_own = .TRUE.
    IF (PRESENT(owns)) should_own = owns

    IF (PRESENT(index)) THEN
      IF (ALLOCATED(this%items)) THEN
        IF (index >=1 .AND. index <= SIZE(this%items)) THEN
          CALL this%items(index)%cleanup()
          this%items(index)%owns_item = should_own
          this%items(index)%item => item
          os_store = index
          RETURN
        END IF
      END IF
    END IF

    IF (.NOT. ALLOCATED(this%items)) THEN
      sz = 1
      ALLOCATE(this%items(1))
    ELSE
      sz = SIZE(this%items)
      ALLOCATE(temp(1:sz+1))
      temp(1:sz) = this%items
      CALL unlink_items(this%items)
      DEALLOCATE(this%items)
      CALL MOVE_ALLOC(temp, this%items)
      sz = sz + 1
    END IF

    this%items(sz)%owns_item = should_own
    this%items(sz)%item => item
    os_store = sz

  END FUNCTION os_store_core



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the ordered store
  !> @param[in] this
  !> @param[in] item
  !> @return os_store
  FUNCTION os_store(this, item, index)

    CLASS(ordered_store), INTENT(INOUT) :: this
    CLASS(*), INTENT(IN) :: item !< Item so store
    !> Index to store the item at. Intended to overwrite an existing item
    !> Not to allow storage in an arbitrary location. If index is not an 
    !> existing item index then the item is simply added to the end of the list
    INTEGER, INTENT(IN), OPTIONAL :: index
    INTEGER(INT32) :: os_store !< Index to which the item is stored
    CLASS(*), POINTER :: item_copy

    ALLOCATE(item_copy, SOURCE = item)

    os_store = this%store_core(item_copy, index)

  END FUNCTION os_store



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the ordered store but hold the pointer rather than own it
  !> @param[in] this
  !> @param[in] item
  !> @param[in] owns
  !> @return os_store
  FUNCTION os_hold(this, item, index, owns)

    CLASS(ordered_store), INTENT(INOUT) :: this
    CLASS(*), POINTER, INTENT(IN) :: item !< Item so store
    !> Index to store the item at. Intended to overwrite an existing item
    !> Not to allow storage in an arbitrary location. If index is not an 
    !> existing item index then the item is simply added to the end of the list
    INTEGER, INTENT(IN), OPTIONAL :: index
    !> Whether the store owns the pointer to the item or not. Optional,
    !> default true
    LOGICAL, INTENT(IN), OPTIONAL :: owns
    INTEGER(INT32) :: os_hold !< Index to which the item is stored

    os_hold = this%store_core(item, index, owns)

  END FUNCTION os_hold


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> If the store is configured to allow it delete an item
  !> @param[in] this
  !> @param[in] item
  !> @return success
  FUNCTION os_delete(this, index) RESULT(success)

    CLASS(ordered_store), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: index
    LOGICAL :: success
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER(INT32) :: sz

    success = this%disorder_level > 0 .AND. ALLOCATED(this%items)
    IF (.NOT. success) RETURN

    IF (index >=1 .AND. index <= SIZE(this%items)) THEN
      CALL this%items(index)%cleanup()
    END IF

    sz = SIZE(this%items)
    ALLOCATE(temp(1:sz-1))
    temp(1:index-1) = this%items(1:index-1)
    temp(index:) = this%items(index+1:)
    CALL unlink_items(this%items)
    DEALLOCATE(this%items)
    CALL MOVE_ALLOC(temp, this%items)

  END FUNCTION os_delete



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Delete all items from the store and deallocate the memory
  !> @param[in] this
  !> @return success
  FUNCTION os_clear(this) RESULT(success)

    CLASS(ordered_store), INTENT(INOUT) :: this
    LOGICAL :: success
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER(INT32) :: iindex

    DEALLOCATE(this%items) 

  END FUNCTION os_clear



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Set disorderable .TRUE.
  !> @param[in] this
  SUBROUTINE os_enable_disorder(this)

    CLASS(ordered_store), INTENT(INOUT) :: this

    this%disorder_level = this%disorder_level + 1

  END SUBROUTINE os_enable_disorder



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Set disorderable .FALSE.
  !> @param[in] this
  SUBROUTINE os_disable_disorder(this)

    CLASS(ordered_store), INTENT(INOUT) :: this

    this%disorder_level = this%disorder_level - 1

  END SUBROUTINE os_disable_disorder



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Insert an item before another item specified by index. Only allowed
  !> if the store has been set to allow disorder. Returns -1 if
  !> insert is not possible (either not disorderable or index out of range)
  !> Can specify an index 1 greater than the count to add at the end
  !> @param[in] this
  !> @param[in] item
  !> @return os_store
  FUNCTION os_insert(this, item, index)

    CLASS(ordered_store), INTENT(INOUT) :: this
    CLASS(*), TARGET, INTENT(IN) :: item !< Item to insert
    !> Item index before which to insert item
    INTEGER, INTENT(IN) :: index
    INTEGER(INT32) :: os_insert !< Index to which the item is stored
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER(INT32) :: sz, old_count

    os_insert = -1
    IF (.NOT. ALLOCATED(this%items)) RETURN
    IF (index < 1 .OR. index > SIZE(this%items) + 1) RETURN

    sz = SIZE(this%items)
    old_count = MIN(sz, index-1)
    ALLOCATE(temp(1:MAX(sz+1, index)))
    temp(1:old_count) = this%items(1:old_count)
    IF (index <= sz) temp(index+1:sz+1) = this%items(index:)
    CALL unlink_items(this%items)
    DEALLOCATE(this%items)
    CALL MOVE_ALLOC(temp, this%items)

    ALLOCATE(this%items(index)%item, SOURCE = item)
    os_insert = index

  END FUNCTION os_insert



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Delete item on destruction
  !> @param[in] this
  PURE ELEMENTAL SUBROUTINE os_destructor(this)

    TYPE(ordered_store), INTENT(INOUT) :: this

    IF (.NOT. ALLOCATED(this%items)) RETURN
    DEALLOCATE(this%items)

  END SUBROUTINE os_destructor

END MODULE eis_ordered_store_mod
