# Using the EIS deck system

## Concepts

The EIS deck system consists of two mandatory components and one optional component. The two mandatory components are 

1. An `eis_deck_definition` object that describes the blocks that can appear in a valid input deck and what functions should be called when a given block is encountered

2. An `eis_text_deck_parser` object that reads a file, deals with comments, line continuation characters etc. and then compares the blocks in the file with the definition and calls the functions associated with the block

Optionally you may also want

3. An `eis_parser` object. One of the options for how the data in the input deck may be made available to the host code is as a preparsed `eis_parser` stack object. If you want a parser more complex than 

## Terms

Parser - A single instance of the `eis_parser` type

Stack - An instance of the `eis_stack` type representing a parsed expression

Definition - An instance of an `eis_deck_definition` object representing a definition for all possible valid decks

Deck object - An instance of an `eis_text_deck_parser` object representing a specific deck in text form

Pass - A specific parsing of a deck. Complete parsing of a deck may involve more than one pass through the deck

Root - The part of a definition representing the highest level of a deck, not inside any blocks

Block - A section of a deck between a `begin : blockname` and an `end : blockname` marker

Key - An item, either inside a block or in the root, having no particular textual form

Key/Value - An item, either inside a block or in the root, having the form `key = value`

Directive - An item, either inside a block or in the root, having the form `directive:type`

Action - An optional host code specified function that is called when certain conditions are met

Start - When `begin : blockname` is encountered then that block starts. Starts may trigger an action

End - When `end : blockname` is encountered then that block ends. Ends may trigger an action

Initialise - When `begin : blockname` is encountered for the first time in a deck. Initialises may trigger an action. Optionally all blocks specified in a definition may be initialised before parsing of a deck object begins

Finalise - When a deck object finishes parsing all blocks that were encountered during parsing of the deck will trigger a finalise action if one is specified. Optionally all blocks specified in the definition will be finialised afer parsing finishes.

Remapping - Specifying that a block of a given name should be treated as a different name for the purposes of defining what actions should happen when that block is encountered

Block definition - A part of a definition that defines actions attached to a specific named block. Will have a unique ID

Block instance - An actual instance of a block encountered when parsing a deck. The ID of a block instance is *not related* to the ID of the definition of that block

ID - Each deck block instance and deck block definition gets a unique ID that is used to uniquely identify it. Host codes normally only need to work with IDs if they have hierarchical blocks that need to know the sequence of blocks in the hierarchy

Event - Events occur when a block starts and ends, when a block cannot be found, when a key is either succesfully or unsuccesfully parsed or when a block or key is not triggered because of pass conditions. You can bind event functions to trigger when any event occurs

Description - A text description of a block or key that may optionally be specified when the block or key is added to the definition

## EIS Deck format

EIS Decks are text files that are intended to control a code. They all have a fairly simple structure which is of separate blocks which contain text key/value pairs. For example

```
begin:block
  key = value
end:block
```

is an extremely simple EIS deck file. You can add comments to an EIS deck file by either putting a `#` character into the text which will cause all of the text from the # to the end of the line to be ignored when parsing the deck or by enclosing a section of text in C comment like `/* Comment */`. In the second case the comment can go over several lines. For example

```
#This is a comment and will be ignored
begin:block
  key = value #This is also a comment and will not be seen when the key is read
end:block

/*
Even though this has the structure of a block it will be ignored because it is enclosed
in multi line comment characters
begin:block2
  key2 = value2
end:block2
*/

```

It is possible to define blocks within other blocks (if the host code chooses to support this). So the following is also a valid block

```
begin:outer_block
  key_outer1 = value_outer1
  key_outer2 = value_outer2
  begin:inner_block
    key_inner = value_inner
  end:inner_block
end:outer_block
```

When you have blocks within blocks like this then all of the keys in the outer block are parsed before the inner block is parsed. It is also possible to have keys that are not in any named block.

All decks include a built in directive called `import`. You use it like

```
begin:outer_block
  key_outer1 = value_outer1
  key_outer2 = value_outer2
  import:inner.deck
end:outer_block
```

and then you define a file called `inner.deck` containing
```  
begin:inner_block
  key_inner = value_inner
end:inner_block
```

and the behaviour will be the same as the previous example. `import` directives can be placed anywhere outside of comments and will have the exact same effect as pasting in the contents of the imported file.

## Definitions vs Decks

It is important when using EIS deck to differentiate between a _deck_ which is an actual file containing information that is used to control a code and a _deck definition_ which describes the things that *could* be in a deck. Before you can parse and actual _deck_ you have to create a _deck definition_ and this README will describe the process of defining a _deck definition_ before it will cover reading an actual deck.

## Deck callback functions

EIS deck definitions work by binding a function to events such as starting or ending a block or encountering a key in a block. These functions come in six forms

### `block_generic_callback` function

The `block_generic callback` function is the type of function that should be supplied as actions for events that are tied to a specifc class of deck blocks but not a specific instance of a deck block. It is used for initialise and finalise events. It is defined as

```fortran
    SUBROUTINE block_generic_callback(block_text, pass_number, parent_kind, &
        status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_generic_callback
```

* `block_text` is the name of the block that triggered this action. Since this function is attached to a class of deck blocks and not a specific block this will always be the name of a block specified in the definition even if you are remapping a block
* `pass_number` is the number of the pass through the deck that is currently happening. This is specified by the host code
* `parent_kind` is an array of definition IDs for all of the block definitions in the heirarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

### `block_callback` function

The `block_callback` function is the type of function that should be supplied as actions for events that are related to a specific instance of a block. Currently these are start and end actions. It is defined as

```fortran
    SUBROUTINE block_callback(block_text, pass_number, parents, parent_kind, &
        status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_callback
```

* `block_text` is the name of the block that triggered this action. This is always the name of the block that was actually present in the deck being parsed, even if you are using a remapping function to select the functions to be called for a block
* `pass_number` is the number of the pass through the deck that is currently happening. This is specified by the host code
* `parents` is an array of instance IDs for all of the blocks in the heirarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the heirarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

### `key_text_callback` function

The `key_text_callback` function is an action function that is called when a key is encountered either in a block or in the root. This action function is the first type that the EIS deck system tries to call for a key and it makes no expectations about the nature of the key beyond it being a section of text. The function is defined as

```fortran
    SUBROUTINE key_text_callback(key_text, pass_number, parents, parent_kind, &
        status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_text_callback
```

* `key_text` is the entire text of the key. Normally in a typical deck this will be of the form "key = value" but this is not enforced
* `pass_number` is the number of the pass through the deck that is currently happening. This is specified by the host code
* `parents` is an array of instance IDs for all of the blocks in the heirarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the heirarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

Usually you will only want to use `key_text_callback` actions to handle unusual deck keys and use the more specific action functions for normal keys. If you get a key and interpret that it doesn't need to be handled by your `key_text_callback` action function then you should set `status_code` to `eis_status_not_handled` to indicate that this action function isn't going to handle this key.

If you are certain that the key will not be handled by any later functions then you can set `errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

### `key_value_callback` function

The `key_value_callback` function is an action funtion that is called when a key is encountered either in a block or in the root. This action function is considered second by the EIS deck system after `key_text_callback`. It only expects that a key is in either the form `key = value` or `key : value` but otherwise 

```fortran
    SUBROUTINE key_value_callback(key_text, value_text, pass_number, parents, &
        parent_kind, status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_value_callback
```

* `key_text` is the part of the key string before the `=` or `:`
* `value_text` is the part of the key string after the `=` or `:`
* `pass_number` is the number of the pass through the deck that is currently happening. This is specified by the host code
* `parents` is an array of instance IDs for all of the blocks in the heirarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the heirarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

You can test for whether the key contained `=` or `:` by testing for `IOR(status_code, eis_status_directive) /= 0`.

If you get a key and interpret that it doesn't need to be handled by your `key_value_callback` action function then you should set `status_code` to `eis_status_not_handled` to indicate that this action function isn't going to handle this key.

If you are certain that the key will not be handled by any later functions then you can set `errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

### `key_numeric_value_callback` 

The `key_numeric_value_callback` function is an action funtion that is called when a key is encountered either in a block or in the root. This action function is considered third by the EIS deck system after `key_value_callback`. It expects that the key is of the form `key = value` or `key : value` and that the value can be parsed into one more numbers (separated by commas if multiple numbers) using the parser specified when the deck object was init-ed.

```fortran
    SUBROUTINE key_numeric_value_callback(key_text, values, pass_number, &
        parser, parents, parent_kind, status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
      INTEGER, INTENT(IN) :: pass_number
      TYPE(eis_parser), INTENT(INOUT) :: parser
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_numeric_value_callback
```

* `key_text` is the part of the key string before the `=` or `:`
* `values` is an array values returned by the parser object after the value is converted to a number
* `pass_number` is the number of the pass through the deck that is currently happening. This is specified by the host code
* `parser` is the parser object that parsed the values
* `parents` is an array of instance IDs for all of the blocks in the heirarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the heirarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

You can test for whether the key contained `=` or `:` by testing for `IOR(status_code, eis_status_directive) /= 0`.

If you get a key and interpret that it doesn't need to be handled by your `key_numeric_value_callback` action function then you should set `status_code` to `eis_status_not_handled` to indicate that this action function isn't going to handle this key.

If you are certain that the key will not be handled by any later functions then you can set `errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

### `key_stack_callback` 

The `key_stack_callback` function is an action funtion that is called when a key is encountered either in a block or in the root. This action function is considered last by the EIS deck system after `key_numeric_value_callback`. It expects that the key is of the form `key = value` or `key : value` and that the value can be parsed into a stack using the parser specified when the deck object was init-ed (or a default parser if no parser was specified).

```fortran
    SUBROUTINE key_stack_callback(key_text, value_stack, pass_number, parser, &
        parents, parent_kind, status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: key_text
      TYPE(eis_stack), INTENT(INOUT) :: value_stack
      INTEGER, INTENT(IN) :: pass_number
      TYPE(eis_parser), INTENT(INOUT) :: parser
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE key_stack_callback
  END INTERFACE
```

* `key_text` is the part of the key string before the `=` or `:`
* `value_stack` is an `eis_stack` object that was tokenized by the parser
* `pass_number` is the number of the pass through the deck that is currently happening. This is specified by the host code
* `parser` is the parser object that tokenized the stack
* `parents` is an array of instance IDs for all of the blocks in the heirarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the heirarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

You can test for whether the key contained `=` or `:` by testing for `IOR(status_code, eis_status_directive) /= 0`.

Although the `key_stack_callback` function is the last action function to be considered for a key it is still valid to set the `status_code` argument to `eis_status_not_handled`. This will cause the fallback `any_key` functions to be called if appropriate or the key to be marked as not recognized if no handler could be found.

If you are certain that the key will not be handled by any later functions then you can set `errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

## Defining a single level deck

The simplest form of deck is the single level or EPOCH style deck. This type of deck consists of all keys being contained in blocks and only one level of block

To define a deck you have to create an instance of `eis_deck_definition` which is defined in the module `eis_deck_definition_mod`. Once you have this object you will usually want to call its `init` method. This is quite a complex method that takes quite a few parameters, all of which are optional. The only one that you will usually want for a single level deck is `parser` which is a pointer to an `eis_parser` object which will be used to parse deck text to a stack or number if requested. If no `parser` parameter is specified then a default parser is created as needed but this will only be able to parse the basic EIS maths expressions. The `init` method returns a pointer to an `eis_deck_block_definition` which describes the root block. It is also defined in the `eis_deck_definition_mod` module. You will generally want to keep a pointer to the root block around but if you ever lose it you can get another by calling the `get_root` method of the definition object.

Once you have your pointer to the root block you will want to call the `add_block` method to add the specific blocks that will appear in your deck. The add block method has a single required parameter `block_name` which is the name of the block to add. The block will be referred to by the name that you give it, and it will be triggered by `begin:block_name` in the deck file when you parse it. `add_block` also has a lot of optional parameters but the ones that would typically be used for a simple deck are

* `init_block` - a `block_generic_callback` function that is called the first time that "start:block\_name" is encountered for this block. It is not called on subsequent "start:block\_name" appearances
* `start_block` - a `block_callback` function that is called each time "start:block\_name" is encountered for this block
* `end_block` - a `block_callback` function that is called each time "end:block\_name" is encountered for this block
* `final_block` - a `block_generic_callback` function that is called when the deck parsing finished for every block that appears at least once in the deck

In fact we can create a simple code using just these pieces, although they are not very useful. This example code does include an actual parse of a deck file that we will cover later.

### Adding blocks

```fortran
MODULE mymod

  USE eis_header
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE init_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, pass_number, parents, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  block => root%add_block('block2', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL deck%init()
  CALL deck%parse_deck_file('demo1.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
  END IF

END PROGRAM testprog
```

If you create a file called demo1.deck containing
```
begin:block1
end:block1

begin:block2
end:block2

begin:block1
end:block1
```
then the output will be

```
 Calling init for block : block1
 Calling start for block : block1
 Calling end for block : block1
 Calling init for block : block2
 Calling start for block : block2
 Calling end for block : block2
 Calling start for block : block1
 Calling end for block : block1
 Calling final for block : block1
 Calling final for block : block2
```

This shows the behaviour of init, start, end and final. If you add an extra block that is not named either `block1` or `block2` then parsing will stop as soon as the incorrect block is encountered and an error will be printed. Note that `final` actions for blocks already encountered before the invalid block will still be called before parsing finishes.

### Adding keys

Each block can have keys in it by adding a key to it. You do this by using the pointer to the `eis_deck_block_definition` object that you get from calls to `add_block` and call the `add_key` method of a block. The `add_key` method takes a name an optionally one or more key handling functions. The key handing functions are called

1. `key_text_fn` - A `key_text_callback` function. This is tried first and simply passes the entire line in the deck to the function for the host code to handle
2. `key_value_fn` - A `key_value_callback` function. This is tried second and splits the line on either an `=` or `:` symbol and passes the two parts as strings to the function
3. `key_numeric_value_fn` - A `key_numeric_value_callback` function. This is tried third and splits the line on `=` or `:` but takes the right hand part and evaluates it in an `eis_parser` object into one or more numeric values. This is mainly intended for using the parser as a calculator for evaluating mathematical expressions into simple values. The expression is evaluated with parser host parameters set to `C_PTR_NULL`.
4. `key_stack_fn` - A `key_stack_callback` function. This is tried last and splits the line on `=` or `:` but takes the right hand part and converts it into and `eis_stack` object that can be evaluated or stored as required by the host code.

`key_text_fn` is intended for very specific types of deck so we recommend only using 2-4. Option 2 which involves you being handed the text for the key and value parts of the expression is the most general. From that part you can parse the text as you want including running it through the `eis_parser` system, but also use values to open files etc. etc.

A very simple example is given below. This reuses the block init, final, start and end calls from the previous examples but adds a function that is called when specific keys are present in the blocks. The keys `key1` and `key2` are added to `block1` and the key `new_key` is added to `block2`

```fortran
MODULE mymod

  USE eis_header
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE init_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, pass_number, parents, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

  SUBROUTINE key_sub(key_text, value_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,' Found in block : ', parents
    PRINT *,' Found in block of type : ', parent_kind
    PRINT *,' Found known key : ', key_text, ' with value ', value_text
  END SUBROUTINE key_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL block%add_key('key1', key_value_fn = key_sub)
  CALL block%add_key('key2', key_value_fn = key_sub)

  block => root%add_block('block2', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL block%add_key('new_key', key_value_fn = key_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo2.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
  END IF

END PROGRAM testprog
```

if you then modify the deck file to read

```
begin:block1
  key2 = mykey
end:block1

begin:block2
  new_key = Hello!
end:block2

begin:block1
  key1 = test_key
end:block1
```

then the output is now

```
 Calling init for block : block1
 Calling start for block : block1
  Found in block :            0           1
  Found in block of type :            0           1
  Found known key : key2  with value  mykey
 Calling end for block : block1
 Calling init for block : block2
 Calling start for block : block2
  Found in block :            0           2
  Found in block of type :            0           2
  Found known key : new_key  with value  Hello!
 Calling end for block : block2
 Calling start for block : block1
  Found in block :            0           3
  Found in block of type :            0           1
  Found known key : key1  with value  test_key
 Calling end for block : block1
 Calling final for block : block1
 Calling final for block : block2
```

You will notice that in this case the same `key_value_callback` function is used for all three keys. This is completely valid and the function gives you enough information to know both which specific block and which kind of block the key was found in. Typically this isn't used much in working code where each key would be bound to a single function that triggers for that key but sometimes there are several keys that have similar enough behaviour that this would cause a lot of duplicated code so this is supported.

Each block definition is given a unique ID when it is added and an array of the IDs for all block definitions leading to the definition of the current block is available in all block init, final, start and end functions and in all key functions. It is in the array `parent_kind`. 0 is the special value for the definition of the root block (outside all named blocks). All blocks have the root block as a parent so the `parent_kind` array always has at least two elements for all named blocks (it is 1 element for keys in the root). The parent kind is printed in this example in the lines reading `Found in block of type`. These show that the first parent is always 0 (the root) but then for blocks of type `block1` the next value is always `1` and for blocks of type `block2` the second value is always ` 2 `. You can retrieve the name of a block from the definition ID using the `get_block_name` method of the `eis_deck_definition` object.

Each instance of a block in a deck is also given a unique ID as well and this ID is not related to the ID of the _definition_ for this type of block. These IDs are available in block start and end functions and in key functions. They are not available in block init and block final functions because these are connected to definitions and not specific blocks. They are in the `parents` array. 0 is the special value for the instance of the root block in the deck and all blocks are children of the root instance so all `parents` arrays have at least 2 elements for named blocks (it has one element for keys in the root). In the example above the `parent` array in the lines reading `Found in block`. You can see here that even blocks having the same _definition_ ID have different _block_ IDs. So the first instance of `block1` has a `parent` ID of 1 and a `parent_kind` ID of 1. This is only a coincidence and the IDs are *not* related. The second instance of `block1` still has a `parent_kind` ID of 1 but the `parent` ID is now 3. You can retreive the name of a block from the unique block ID using the `get_block_name` method of the `eis_text_deck_parser` object, but many codes will want to record this ID in some other way, especially if you are using a heirarchical deck. NOTE : The ID for each block is assigned depending on that block's position in the deck file. The same block appearing at a different point in the deck will have a different ID. If you want to uniquely identify a block so that it can always be identified regardless of its position in the deck file you should set up a definition where the user gives the block a unique name key that the host code should track.

### Numerical keys

The next most common thing to want to do is to parse the keys to numerical values. This is done using `key_numeric_value_callback` function passed to the `key_numerical_value_fn` parameter to `add_key`. To demonstrate that the parser has converted the text to a number this example both prints the value and the value - 1

```fortran
MODULE mymod

  USE eis_header
  USE eis_parser_mod
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE init_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, pass_number, parents, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

  SUBROUTINE key_sub(key_text, values, pass_number, cap_bits, parser, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_bitmask), INTENT(IN) :: cap_bits
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
    TYPE(eis_parser), INTENT(INOUT) :: parser
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Found key ', TRIM(key_text), '. It has ', SIZE(values), 'elements'
    PRINT *,'Value of first result is ', values(1)
    PRINT *,'Value of first result -1 is ', values(1) - 1.0_eis_num

  END SUBROUTINE key_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL block%add_key('key1', key_numeric_value_fn = key_sub)
  CALL block%add_key('key2', key_numeric_value_fn = key_sub)

  block => root%add_block('block2', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block)

  CALL block%add_key('new_key', key_numeric_value_fn = key_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo3.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
```

an example deck that would go with this is

```
begin:block1
  key2 = 10
end:block1

begin:block2
  new_key = (10+20)/20
end:block2

begin:block1
  key1 = sin(pi/2)
end:block1
```

### Mixed keys

A common thing to want to do is parse some kinds of keys as strings and some as numbers. Because the text parser runs before the numeric parser you have to use some kind of mechanism to detect which keys should be handled as text. There are a lot of ways to do this but for this example we're just going to enclose strings in quotes.

```fortran
MODULE mymod

  USE eis_header
  USE eis_parser_mod
  USE eis_deck_header
  IMPLICIT NONE
  CONTAINS


  SUBROUTINE key_str_sub(key_text, key_value, pass_number, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    CHARACTER(LEN=*), INTENT(IN) :: key_value
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: lq, uq

    !If no quotes are present then this is not a bad value
    lq = INDEX(key_value, '"')
    IF (lq == 0) THEN
      status_code = eis_status_not_handled
      RETURN
    END IF

    uq = INDEX(key_value, '"', .TRUE.)
    !Otherwise check that there are two quotes and that there is at least
    !a single character between them. If not then this is a bad value
    IF (uq - lq < 2) THEN
      errcode = eis_err_bad_value
      RETURN
    END IF

    PRINT *,'Found text key ', TRIM(key_text), '. Value is ', &
        key_value(lq+1:uq-1)

  END SUBROUTINE key_str_sub


  SUBROUTINE key_val_sub(key_text, values, pass_number, cap_bits, parser, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_bitmask), INTENT(IN) :: cap_bits
    TYPE(eis_parser), INTENT(INOUT) :: parser
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Found numerical key ', TRIM(key_text), '. Values are ', values

  END SUBROUTINE key_val_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str, serial_deck
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1')

  CALL block%add_key('key1', key_value_fn = key_str_sub, &
      key_numeric_value_fn = key_val_sub)
  CALL block%add_key('key2', key_value_fn = key_str_sub, &
      key_numeric_value_fn = key_val_sub)

  block => root%add_block('block2')

  CALL block%add_key('new_key', key_value_fn = key_str_sub, &
      key_numeric_value_fn = key_val_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo4.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
```

The function `key_str_sub` checks for the first and last instances of `"` in a key. It then checks if either there are either

1. No `"` characters in the value string. In this case it sets `status` to `eis_status_not_handled` because this flags that it should try to parse the value as a maths parser expression

2. Only one `"` in the value string or two that have no space between them. In this case it sets `errcode` to `eis_err_bad_value` because this is a string that it should try to treat as a string but wasn't able to validly

If neither of these conditions is true then it simply prints the string that it has found and does nothing else. Parsing will not proceed to the maths parser for this key.

If you then run this on a suitable deck
```
begin:block1
  key2 = "my key"
end:block1

begin:block2
  new_key = 7+12
end:block2

begin:block1
  key1 = sin(pi/3)
end:block1
```

you will get the output

```
 Found text key key2. Value is my key
 Found numerical key new_key. Values are    19.000000000000000     
 Found numerical key key1. Values are   0.86602540378443860  
```

For keys where text parsing isn't possible then simply don't specify the `key_value_fn` parameter when creating the key and the parser will only try to parse that key to values. Similarly if you don't specify `key_numeric_value_fn` then only the text parsing will occur.

If you want to try parsing the string to a number and then interpreting it as a string if that fails you have to do this manually. Set an action function to get the text version of the value and then evaluate that manually to an `eis_parser` object. If you detect an error from that evaluation then you should interpret the value as a string. Note that in this case you will probably want to call the `flush_errors` method of the parser after the parse to clear the error entries in the error log for the parser.

### Stack keys

The more general version of numerical keys are stack keys. This mode of parsing will be tried last, after all other modes are rejected either by not having an action function associated with them or if they all return a status of `eis_status_not_handled`. This mode is very similar to parsing keys to numeric values but the action function is called with an `eis_stack` object rather than the numerical values. This is more flexible in various ways.

1. You can pass host parameters when parsing the expression
2. You can evaluate the stack multiple times (often with different host parameters)
3. You can copy the stack so that you can keep it and evaluate it in the future if wanted

### "Any" keys

As well as having specific action functions for known keys, EIS deck also offers the options of having action functions that trigger for any keys that either don't have specific action functions associated with them or where all of the specific action functions return the `eis_status_not_handled` status. Typically you would use "any" key handlers for dealing with keys where part of their name has to be 

You specify "Any" action functions when you create a block in the `add_block` function. You set 

1. `any_key_text` - any version of `key_text_fn`
2. `any_key_value` - any version of `key_value_fn`
3. `any_key_numeric_value` - any version of `key_numeric_value_fn`
4. `any_key_stack` - any version of `key_stack_fn`

"Any" functions have the same signature as the normal versions and behave in the same way, but you are essentially required to check the `key_text` parameters to make sure that you are handling a key that makes sense.

```fortran
MODULE mymod

  USE eis_header
  USE eis_parser_mod
  USE eis_deck_header
  IMPLICIT NONE
  CONTAINS


  SUBROUTINE key_str_sub(key_text, key_value, pass_number, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    CHARACTER(LEN=*), INTENT(IN) :: key_value
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: lq, uq

    !If no quotes are present then this is not a bad value
    lq = INDEX(key_value, '"')
    IF (lq == 0) THEN
      status_code = eis_status_not_handled
      RETURN
    END IF

    uq = INDEX(key_value, '"', .TRUE.)
    !Otherwise check that there are two quotes and that there is at least
    !a single character between them. If not then this is a bad value
    IF (uq - lq < 2) THEN
      errcode = eis_err_bad_value
      RETURN
    END IF

    PRINT *,'Found text key ', TRIM(key_text), '. Value is ', &
        key_value(lq+1:uq-1)

  END SUBROUTINE key_str_sub


  SUBROUTINE key_val_sub(key_text, values, pass_number, cap_bits, parser, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_bitmask), INTENT(IN) :: cap_bits
    TYPE(eis_parser), INTENT(INOUT) :: parser
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Found numerical key ', TRIM(key_text), '. Values are ', values

  END SUBROUTINE key_val_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', any_key_value = key_str_sub, &
      any_key_numeric_value = key_val_sub)

  block => root%add_block('block2')

  CALL block%add_key('new_key', key_value_fn = key_str_sub, &
      key_numeric_value_fn = key_val_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo5.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
```

In this example `block1` uses any key functions while `block2` still uses specific named keys. Block 1 will now accept any keys in the block rather than simple named keys.

If `any` functions are used then the error that is reported when a key fails to be recognized by all of the `any` functions depending on whether or not the key has a specific action function for it too. If a key has no specific action functions associated with it then when the last `any` function fails an `eis_err_unknown_key` error occurs. If a key has a specific action function associated with it then when the last any function fails an `eis_err_bad_key` error occurs.

### Defining a multi level deck definition

Decks that have blocks defined within blocks are called multi-level decks. Multi level decks are specified in the same way as single level decks, but you call the `add_block` method on a block rather than on the root item. The function is exactly the same and takes all of the same parameters.

```fortran
MODULE mymod

  USE eis_header
  USE eis_parser_mod
  USE eis_deck_header
  IMPLICIT NONE
  CONTAINS


  SUBROUTINE key_str_sub(key_text, key_value, pass_number, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    CHARACTER(LEN=*), INTENT(IN) :: key_value
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    INTEGER :: lq, uq

    !If no quotes are present then this is not a bad value
    lq = INDEX(key_value, '"')
    IF (lq == 0) THEN
      status_code = eis_status_not_handled
      RETURN
    END IF

    uq = INDEX(key_value, '"', .TRUE.)
    !Otherwise check that there are two quotes and that there is at least
    !a single character between them. If not then this is a bad value
    IF (uq - lq < 2) THEN
      errcode = eis_err_bad_value
      RETURN
    END IF

    PRINT *,'Found text key ', TRIM(key_text), '. Value is ', &
        key_value(lq+1:uq-1)

  END SUBROUTINE key_str_sub


  SUBROUTINE key_val_sub(key_text, values, pass_number, cap_bits, parser, &
      parents, parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    REAL(eis_num), DIMENSION(:), INTENT(IN) :: values
    INTEGER, INTENT(IN) :: pass_number
    INTEGER(eis_bitmask), INTENT(IN) :: cap_bits
    TYPE(eis_parser), INTENT(INOUT) :: parser
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Found numerical key ', TRIM(key_text), '. Values are ', values

  END SUBROUTINE key_val_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', any_key_value = key_str_sub, &
      any_key_numeric_value = key_val_sub)

  block => block%add_block('block2')

  CALL block%add_key('new_key', key_value_fn = key_str_sub, &
      key_numeric_value_fn = key_val_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo6.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
```
This example is the same as the previous one but moves block2 to be a sub-block of block1. A suitable deck would now look like

```
begin:block1
  key2 = "my key"

  begin:block2
    new_key = 7+12
  end:block2
end:block1

begin:block1
  key1 = sin(pi/3)
end:block1

```

And this will produce the same output as the previous example. Putting `block2` out in the root block is now invalid and will produce the output

```
Found text key key2. Value is my key
================================================================================
Error when parsing an input deck file

In file "test.deck" on line  5 : An unknown block was found in the deck
================================================================================
```

because block2 is only a valid sub-block of block1. If you wanted block2 to be in both block1 and the root block then you would have to add a block called "block2" to both block1 and the root block with the same action functions. It is important to note that this blocks will still be distinct and have different unique `parent_kind` definition ID numbers but the host code can ignore this fact and make both blocks behave identically if wanted.s

In multi level decks there is a strict parsing order.

1. All keys at each level are parsed first in the order that they appear in the deck
2. All blocks are then parsed in the order they appear in the deck
3. If a block contains sub-blocks then all of its sub-blocks are parsed to full depth before the next block is parsed

### Block remapping

Because blocks have a strict heirarchy that is described through their unique ID keys there is no direct equivalent to `any` action functions for blocks. The equivalent for blocks is called `block remapping` and works by taking a block with an unknown name and stating what block type it should be treated as by specifying the name of the block. Block remapping is a block by block level process with each block (including the root block) having an optional remapping function that is used if an unknown block is encountered in that block. You might want to use block remapping if you want a user to be able to use the name of something they specified in the name of a block. For example

```
begin:object
  object_name = my_object
end:object

begin:output_my_object

end:output_my_object
```

as a way of specifying output options for a named object. You could implement this in other ways in an EIS deck (either have an output block within the object block or have an object_name key in the output block to tie the block to they key) but this allows a developer to use user specified text in input deck block names.

Block remapping functions have the following signature

```fortran
    SUBROUTINE block_remap_callback(block_text, pass_number, remap_text, &
        status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: block_text
      INTEGER, INTENT(IN) :: pass_number
      CHARACTER(LEN=*), INTENT(INOUT) :: remap_text
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE block_remap_callback
```

The `block_text` argument will contain the block name found in the deck file. `remap_text` is an empty string that is large enough to contain the largest possible block that you could remap to. You should fill it with the name of the block that you should treat this block as. If you cannot remap the block name then you should set `status_code` to `eis_status_not_handled`.

You set the remapping function by specifying the optional `block_remapper` argument as a function with the signature of a `block_remap_callback` to either a call to the `add_block` method of an `eis_deck_block_definition` object (describing a block) or the `init` method of an `eis_deck_defintion` object where it is applied to the root block object.

```fortran
MODULE mymod

  USE eis_header
  USE eis_parser_mod
  USE eis_deck_header
  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  IMPLICIT NONE
  SAVE
  TYPE(eis_deck_definition) :: dfn
  TYPE(eis_text_deck_parser) :: deck
  CONTAINS

  SUBROUTINE remapper(block_text, pass_number, remap_text, status_code, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    CHARACTER(LEN=*), INTENT(INOUT) :: remap_text
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    IF (INDEX(block_text, "block") /= 0) THEN
      remap_text = "generic_block"
    ELSE
      status_code = eis_status_not_handled
    END IF
  END SUBROUTINE remapper

  SUBROUTINE init_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, pass_number, parents, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

  SUBROUTINE key_sub(key_text, value_text, pass_number, parents, &
      parent_kind, status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text
    CHARACTER(LEN=*), INTENT(IN) :: value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode
    CHARACTER(LEN=:), ALLOCATABLE :: name

    PRINT *, 'Found key ', TRIM(key_text), ' with value ', TRIM(value_text)
    CALL dfn%get_block_name(parent_kind(SIZE(parent_kind)), name)
    PRINT *, 'Block generic name for key is ', name
    CALL deck%get_block_name(parents(SIZE(parents)), name)
    PRINT *, 'Block specific name for key is ', name

  END SUBROUTINE key_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init(block_remapper = remapper)
  block => root%add_block('generic_block', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block, any_key_value = key_sub)

  CALL deck%init()
  CALL deck%parse_deck_file('demo7.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE.)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    DEALLOCATE(str)
  END IF

END PROGRAM testprog
```

In this example any block beginning with the word `block` is remapped to use the action functions associated with the `generic_block` block, so the example deck, so the test deck

```
begin:block_1
  key2 = "my key"
end:block_1

begin:block_2
  new_key = 7+12
end:block_2

begin:block_1
  key1 = sin(pi/3)
end:block_1
```

will produce the output

```
 Calling init for block : generic_block
 Calling start for block : block_1
 Found key key2 with value  "my key"
 Block generic name for key is generic_block
 Block specific name for key is block_1
 Calling end for block : block_1
 Calling start for block : block_2
 Found key new_key with value  7+12
 Block generic name for key is generic_block
 Block specific name for key is block_2
 Calling end for block : block_2
 Calling start for block : block_1
 Found key key1 with value  sin(pi/3)
 Block generic name for key is generic_block
 Block specific name for key is block_1
 Calling end for block : block_1
 Calling final for block : generic_block
```

As you can see from that output when doing block remapping there is a slight change to the behaviour of the init/start/end/final action functions. The init and final action functions are not associated with any particular block so they are called with a `block_text` parameter of the generic block name that is being remapped to. The start and end action functions are specific to a given block so they are passed the text of the block that is actually being encountered in the deck without name remapping. This is how you write the code to be able to deal with different blocks having different behaviours. Since you will often not want the generic block name to be a valid block in the deck you would want to test for the generic name being passed as the `block_text` parameter to the `start` action function and return `eis_err_unknown_block` error if it is found.

### Pass triggering
When you call to parse a deck you can specify the `pass_number`. This is used to specify that you intend to run through your deck multiple times, presumably doing different things each time. It is associated with another concept `max_passes` which says the number of times that you intend to run through your deck. Block initialisation only happens on pass number 1 and block finalisation only automatically on pass number `max_passes`, but you can also specify certain blocks or keys should only be processed on certain pass numbers. When you create a block or a key you can optionally pass one of the following integer parameters which will restrict which passes a key or block should trigger on.

1. pass_eq - Block should trigger on this pass number
2. pass_le - Block should trigger if pass number is less than or equal to the parameter
3. pass_ge - Block should trigger if pass number is greater than or equal to the parameter

If you specify multiple of these parameters then they are combined as OR, so `pass_le = 3, pass_ge = 5` would mean "trigger for all passes except pass 4". You cannot at present specify more complex conditions and should use the `pass_number` parameter to the callback functions and have the function triggered for all passes if you want more complex behaviour.

On a pass which is flagged to not trigger a key they key is not processed. On a pass which is flagged to not trigger a block then block start, init and end behaviours will not trigger. Finalise will occur on the final pass regardless of the block being marked to trigger on the final pass or not. The behaviour is hierarchical by default, so all keys in a block and all subblocks of a block inherit it's pass triggering behaviour by default. Manually specifying a behaviour will override the behaviour from the parent. Thus, a block which is not triggered on a particular pass may have individual sub-blocks or keys that will trigger, but it will *not* trigger any `any key` behaviour for that block. Block remapping will occur under all circumstances and the pass triggering behaviour of the remapped block is the behaviour of the block that it is remapped to. An example of pass triggering is

```fortran
MODULE mymod

  USE eis_header
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE init_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling init for block : ', block_text
  END SUBROUTINE init_block

  SUBROUTINE final_block(block_text, pass_number, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling final for block : ', block_text
  END SUBROUTINE final_block

  SUBROUTINE start_block(block_text, pass_number, parents, parent_kind, &
      status, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling start for block : ', block_text
  END SUBROUTINE start_block

  SUBROUTINE end_block(block_text, pass_number, parents, parent_kind, status, &
      host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: block_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,'Calling end for block : ', block_text
  END SUBROUTINE end_block

  SUBROUTINE key_sub(key_text, value_text, pass_number, parents, parent_kind, &
      status_code, host_state, errcode)
    CHARACTER(LEN=*), INTENT(IN) :: key_text, value_text
    INTEGER, INTENT(IN) :: pass_number
    INTEGER, DIMENSION(:), INTENT(IN) :: parents
    INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
    INTEGER(eis_status), INTENT(INOUT) :: status_code
    INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
    INTEGER(eis_error), INTENT(INOUT) :: errcode

    PRINT *,' Found in block : ', parents
    PRINT *,' Found in block of type : ', parent_kind
    PRINT *,' Found known key : ', key_text, ' with value ', value_text
  END SUBROUTINE key_sub

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block, pass_eq = 1)

  CALL block%add_key('key1', key_value_fn = key_sub)
  CALL block%add_key('key2', key_value_fn = key_sub)

  block => root%add_block('block2', init_block = init_block, &
      start_block = start_block, end_block = end_block, &
      final_block = final_block, pass_eq = 2)

  CALL block%add_key('new_key', key_value_fn = key_sub)

  CALL deck%init()
  PRINT *,'*** First pass through deck ***'
  CALL deck%parse_deck_file('demo8.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE., max_passes = 2, pass_number = 1)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
  END IF

  PRINT *,'*** Second pass through deck ***'
  CALL deck%parse_deck_file('demo8.deck', dfn, errcode, &
      allow_empty_blocks = .TRUE., max_passes = 2, pass_number = 2)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
  END IF

END PROGRAM testprog
```

with the deck

```
begin:block1
  key2 = mykey
end:block1

begin:block2
  new_key = Hello!
end:block2

begin:block1
  key1 = test_key
end:block1
```

### Assignment variables

The final way in which EIS can report key values from a deck is straight into a numerical variable. This requires that a key be of key/value or directive form. The output can be placed into a POINTER variable of the following types : 32 bit integer, 64 bit integer, 32 bit real, 64 bit real default kind logical or any rank 1 array of those types. When they key is encountered it is automatically converted into a numerical representation and placed into the associated variable. Assignment to a scalar variable will only succeed if the key evaluates to a single value and assignment to a rank 1 array will only succeed if the array is at least large enough to hold all of the returned values. If the array is larger than the number of returned values then the values are populated from lowest to highest. If the parser was compiled with F2008 support then TARGET variables can be used as well as POINTER. If assignment variables are combined with action functions then they are considered after all action functions if no action function has handled the key but before any `any` functions are triggered.

i32value, i64value, r32value, &
      r64value, logicalvalue, i32array, i64array, r32array, r64array, &
      logicalarray

To return a value from a key to an assignment variable you specify one of the following parameters to the `add_key` function, setting the value to the variable to hold the result for the key

1. i32value - Assign return value to the specified 32 bit integer scalar
2. i64value - Assign return value to the specified 64 bit integer scalar
3. r32value - Assign return value to the specified 32 bit real scalar
4. r64value - Assign return value to the specified 64 bit real scalar
5. logicalvalue - Assign return value to the specified default kind logical scalar
6. i32array - Assign return value to the specified 32 bit integer rank 1 array
7. i64array - Assign return value to the specified 64 bit integer rank 1 array
8. r32array - Assign return value to the specified 32 bit real rank 1 array
9. r64array - Assign return value to the specified 64 bit real rank 1 array
10. logicalarray - Assign return value to the specified default kind logical rank 1 array

The following example code shows a simple use of assignment variables

```fortran
MODULE mymod
  
  USE eis_header
  IMPLICIT NONE
  SAVE

  !Must be a POINTER variable in F2003
  !Can be TARGET in F2008
  !MUST NOT be a plain variable
  INTEGER, POINTER :: scalar
  REAL, DIMENSION(:), POINTER :: array

END MODULE mymod


PROGRAM testprog

  USE eis_deck_definition_mod
  USE eis_deck_from_text_mod
  USE eis_deck_header
  USE mymod
  IMPLICIT NONE

  TYPE(eis_text_deck_parser) :: deck
  TYPE(eis_deck_definition) :: dfn
  INTEGER(eis_error) :: errcode
  TYPE(eis_deck_block_definition), POINTER :: root, block
  CHARACTER(LEN=:), ALLOCATABLE :: str
  INTEGER :: ierr

  ALLOCATE(scalar)
  ALLOCATE(array(4))

  errcode = eis_err_none
  root => dfn%init()
  block => root%add_block('block1')
  CALL block%add_key('scalar_value', i32value = scalar)
  CALL block%add_key('array_value', r32array = array)

  CALL deck%init()
  CALL deck%parse_deck_file('demo10.deck', dfn, errcode)
  IF (errcode /= eis_err_none) THEN
    DO ierr = 1, deck%get_error_count()
      CALL deck%get_error_report(ierr, str)
      PRINT *, str
    END DO
    STOP
  END IF

  PRINT *, 'Scalar value is ', scalar
  PRINT *, 'Array value is ', array

  DEALLOCATE(scalar)
  DEALLOCATE(array)

END PROGRAM testprog
```

and associated deck

```
begin:block1
  scalar_value = 1
  array_value = 1,2,3,4
end:block1
```

Assignment variables are very powerful but come with a variety of problems. If a pointer variable is reassigned then the deck key associated with it must be recreated as well or the code will crash when that key is encountered. Also assuming that you are reading variables from the deck for use as your code runs performance of your core code may well be lower using pointer or target variables than using a normal or allocatable variable and using an action function to set it's values.

### Event functions

You can bind event functions to a deck definition that will trigger when an event occurs. The event functions are different to action functions because they are triggered by a *type* of action occuring rather than being connected to a specific named block or key. You bind event functions in the `init` function for the deck definition. There are 7 event functions

1. `on_key_success` which occurs when a key is succesfully parsed
2. `on_key_failure` which occurs when a key has not been able to be parsed, either due to the key not being known (and not being handled by an `any_key` function) or due to a failure in key handling
3. `on_key_no_trigger` which occurs when a key has not been triggered because of pass triggering rules
4. `on_block_start` which occurs when a block starts. It occurs for both non-remapped and remapped blocks
5. `on_block_end` which occurs when a block ends. It occurs for both non-remapped and remapped blocks
6. `on_block_no_trigger` which occurs when a block would start but does not because of pass triggering rules. There is no matching untriggered block end
7. `on_block_failure` which occurs when an error occured when starting or ending a block. Usually this is because a block was not found but this even will trigger if any error is reported during a block start or a block end

All of the event functions are of the same type

```fortran
    SUBROUTINE event_callback(event_text, pass_number, parents, parent_kind, &
        status_code, host_state, errcode)
      CHARACTER(LEN=*), INTENT(IN) :: event_text
      INTEGER, INTENT(IN) :: pass_number
      INTEGER, DIMENSION(:), INTENT(IN) :: parents
      INTEGER, DIMENSION(:), INTENT(IN) :: parent_kind
      INTEGER(eis_status), INTENT(INOUT) :: status_code
      INTEGER(eis_bitmask), INTENT(INOUT) :: host_state
      INTEGER(eis_error), INTENT(INOUT) :: errcode
    END SUBROUTINE event_callback
```

The data that is passed to the event functions is mostly always the same, but there are a few differences. The `event_text` parameter contains the name of the block that caused the event for block functions and the entire text (key and value) for key functions. The `parents` and `parent_kind` parameters contain the same information as the equivalent parameters for the action functions _except_ for when the `on_block_failure` event is triggered because of an unknown block in which case both arrays are of zero length. The `errcode` parameter will ususually be `eis_err_none` but in the case of `on_block_failure` and `on_key_failure` events it will be populated with the error code that caused the failure. Overriding this error code will replace the error originally reported. In particular setting it to 'eis_err_none' will prevent the error from triggering.

Event functions are intended for a variety of purposes but the main purpose is to allow for simple reporting of keys and blocks in a deck. You might, for example, bind event functions to `on_block_start`, `on_block_end`, `on_key_success` and `on_key_failure` to simply print the blocks and keys encountered into a file to help with reporting status if a user presents a malformed deck.

### Descriptions

You can optionally specify a description of the meaning and function of a block or key when you add the block or key to the definition. This is done by specifying a `definition` optional argument to the `add_block` or `add_key` methods. The descriptions are mainly retreived through the `get_block_structure_as_markdown` method. This method takes an allocatable character variable and returns a common markdown document describing all of the blocks in the definition

## Loading and parsing a deck

Once you have a deck definition created you will want to actually use it to parse a deck. The previous examples have shown how to do this - by creating an `eis_text_deck_parser` object and using it to read a text file. The method that you will usually want to use is `parse_deck_file`. This takes 3 compulsory parameters

1. String containing name of the deck file to load
2. Instance of `eis_deck_definition` to parse the deck using. This definition *must* have already been set up
3. `INTEGER(eis_error)` to hold any error codes from the parse. If this is not `eis_err_none` then you should check the error report

It also has 9 optional paramaterers

1. `allow_root_keys` - Logical parameter for whether keys in the root level (outside all blocks) should be permitted. Default .FALSE.
2. `allow_empty_blocks` - Logical parameter for whether blocks that contain no keys is permitted or not. Default .FALSE.
3. `unknown_block_is_fatal` - Logical parameter for whether to finish parsing when an unknown block is encountered. Default .TRUE.
4. `unknown_key_is_fatal` - Logical parameter for whether to finish parsing when an unknown key is encountered. Default .TRUE.
5. `initialise_all_blocks` - Logical parameter for whether to initialise all blocks that are known by the parser at the start of the parse rather than when the first block is encountered. Default .FALSE. and blocks are initialised as they are encountered in the deck.
6. `max_level` - The maximum depth of blocks in the deck parser. If 0 then only keys in the root block are permitted, if 1 then only one level of blocks are permitted, if 2 then those blocks can have sub-blocks, but the sub-blocks may not themselves have sub-blocks, etc. etc. If not specified then blocks as deep as they are defined in the definition are permitted
7. `pass_number` - Allows multiple passes of a deck and should be used with the `max_passes` optional parameter. If `pass_number` is present and set to `1` then blocks will be initialised as they are encountered (or as requested in `initialise_all_blocks`). If `pass_number` is present and equal to `max_passes` then blocks that were encountered will be finalized at the end of the call. If both are present but they are not equal then no finalization will be performed. This is aimed at dealing with host codes that need to deal with parsing the deck multiple times, usually becuase information is needed to set up arrays or other data structures before they are populated. For host codes that may have an uncertain number of passes through a deck file you should set `max_passes` to a number large enough for the maximum number of passes and call the `finalize_deck` method when parsing is complete.
8. `max_passes` - Specifies the maximum number of times that the deck will be parsed. Used in conjunction with `pass_number`. If `pass_number` equals `max_passes` then deck blocks will be finalized when this call to the function finishes, otherwise they will not be. This is aimed at dealing with host codes that need to deal with parsing the deck multiple times, usually becuase information is needed to set up arrays or other data structures before they are populated. For host codes that may have an uncertain number of passes through a deck file you should set `max_passes` to a number large enough for the maximum number of passes and call the `finalize_deck` method when parsing is complete.
9. `state` - An arbitrary integer of kind `eis_bitmask`. It's intended to allow the host code a simple way of flagging internal states. The value passed to this paramater is passed in to all of the action functions for both blocks and keys and may be modified there. It is "intended" that this will be a state bitmask (hence the name of the kind) but it could be used to refer to an item in an array or similar mechanism. Obviously it is also possible for a host code to use a mechanism not connected to EIS to report errors and or statuses.

The `parse_deck_file` method loads the specified file, deals with whitespace, comment characters and imported files and then splits into blocks and keys and compares those blocks and keys with the definition and calls the appropriate action functions.

The next most common way of handing a deck to EIS is using the `parse_deck_string` method. This is the same as `parse_deck_file` but takes a string containing an entire input deck rather than a filename. There is also an extra optional parameter

1. filename - Optional string saying the name of the file that the string provided to `parse_deck_string` came from. If provided then the error reporting will include the name of this file in the errors but otherwise has no effect.

Finally there is a convenient way of dealing with the case of a distributed code that wants to load a file (including import directives) on one processor and distribute it to other processors while keeping the helpful error reporting etc. This introduce two new methods - one to load the file on one processor in a way that fully describes inclued files etc. and one to parse this loaded data as a deck.

To load the file you call `get_deck_serialised` usually only on a single processor. This function takes

1. A CHARACTER variable or literal holding the filename of the deck file to load
2. A `CHARACTER(LEN=:), ALLOCATABLE` variable to hold the loaded, serialised data
3. An `INTEGER(eis_error)` variable to hold the error code. If this is not `eis_err_none` after the call then you should test the errors from the parser

The result of calling this function (if it succeeds) is to allocate the character variable to hold a version of the file specified on the disk, including enough information to reconstruct information about included files, line numbers etc. This is guaranteed to be an ASCII string that can be sent through normal communication channels.

`get_deck_serialised` *ONLY* produces the serialised version of the deck. It doesn't parse the deck in any way because this would cause problems for codes that need explicit synchronization in their action functions. To parse the deck that you got from `get_deck_serialised` you have to use the `parse_deck_serialised` method. This is almost identical to `parse_deck_file` and `parse_deck_string` but the CHARACTER parameter that is the filename in `parse_deck_file` and the text to parse in `parse_deck_string` should be replaced with the output from `get_deck_serialised`.

## Threads and the deck parser
The deck parser is necessarily stateful. At present it is not thread safe and you must have one parser object and one deck definition per active thread. 

## Error reporting

The `eis_text_parser` object has built in reporting of errors as shown in the example code already given. To find out the number of errors call the `get_error_count` function. To get the report for each error call the `get_error_report` function. This tells you the location of all of the errors that were encountered in both the deck parsing and any maths parsing of keys that occurs. If an error occurs when parsing a maths expression this is will appear as two errors, one for the error in the maths parser and then one for the key where the error occurs
