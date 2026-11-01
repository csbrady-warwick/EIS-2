# Using the EIS deck system from C

## Optional parameters

Many of the functions in the C interface have optional behaviour. In the Fortran interface this is done with optional arguments. In the C interface a function that has options takes a pointer to a structure that holds them as its last argument, and this document calls the members of these structures optional parameters. When a description in this document says that a function takes "the parameters" it means this structure.

### Finding the names

The structure for a function has the name of the function followed by `_params`, so the optional parameters of `eis_deck_add_block` are held in a structure of type `eis_deck_add_block_params`. The function that sets the defaults for a structure has the name of the structure followed by `_init`, so here it is `eis_deck_add_block_params_init`. The members of each structure, and what they do, are described in the API documentation for the structure and in `eis_deck.h`.

### Setting them

If you pass `NULL` as the pointer then every optional parameter has its default value. To change one or more of them, declare the structure, set it to the defaults with its `_init` function, change the members that you want and pass a pointer to it.

```c
eis_deck_add_block_params block_params;
eis_deck_add_block_params_init(&block_params);
block_params.start_block = start_block;
block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);
```

Always call the `_init` function before you change anything rather than filling in the structure yourself. Members that you do not set would otherwise hold arbitrary values, and the defaults of members that are added in the future are only set by `_init`. Members that are function pointers are `NULL` when they are not specified. Integer members such as `pass_eq` or `text_fallback` use -1 to mean that the value is not specified. The structure, and any strings that it points to, only need to exist for the duration of the call, so you can change a structure and use it again for another call. Functions that have no options at present still take the argument, so pass `NULL` for those.

### How this document shows them

Whenever the text or an example uses an optional parameter it names the parameter, and code examples that set optional parameters have a comment saying so. Arguments that are not optional parameters, such as the name of a block, are ordinary arguments of the function.

## Concepts

The EIS deck system consists of two mandatory components and one optional component. The two mandatory components are

1. A deck definition that describes the blocks that can appear in a valid input deck and what functions should be called when a given block is encountered. You create one with `eis_deck_create_definition`, which returns an `int` handle

2. A deck parser that reads a file, deals with comments, line continuation characters etc. and then compares the blocks in the file with the definition and calls the functions associated with the block. You create one with `eis_deck_create_parser`, which returns an `int` handle

Optionally you may also want

3. A maths parser created by `eis_create_parser`. One of the options for how the data in the input deck may be made available to the host code is as a preparsed stack. If you want a parser more complex than the default one then create your own parser and give its handle to the deck definition

## Terms

Parser - A maths parser, referred to by an `int` handle

Stack - A parsed expression, referred to by an `int` handle

Definition - A deck definition representing a definition for all possible valid decks, referred to by an `int` handle

Deck object - A deck parser representing a specific deck in text form, referred to by an `int` handle

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

Finalise - When a deck object finishes parsing all blocks that were encountered during parsing of the deck will trigger a finalise action if one is specified. Optionally all blocks specified in the definition will be finalised after parsing finishes.

Remapping - Specifying that a block of a given name should be treated as a different name for the purposes of defining what actions should happen when that block is encountered

Block definition - A part of a definition that defines actions attached to a specific named block. Will have a unique ID

Block instance - An actual instance of a block encountered when parsing a deck. The ID of a block instance is *not related* to the ID of the definition of that block

ID - Each deck block instance and deck block definition gets a unique ID that is used to uniquely identify it. Host codes normally only need to work with IDs if they have hierarchical blocks that need to know the sequence of blocks in the hierarchy

Event - Events occur when a block starts and ends, when a block cannot be found, when a key is either successfully or unsuccessfully parsed or when a block or key is not triggered because of pass conditions. You can bind event functions to trigger when any event occurs

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

It is important when using EIS deck to differentiate between a _deck_ which is an actual file containing information that is used to control a code and a _deck definition_ which describes the things that *could* be in a deck. Before you can parse an actual _deck_ you have to create a _deck definition_ and this README will describe the process of defining a _deck definition_ before it will cover reading an actual deck.

## Deck callback functions

EIS deck definitions work by binding a function to events such as starting or ending a block or encountering a key in a block. These functions come in six forms. In all of them the text is a `char*`, the pass number is passed as a pointer to an `int` and the arrays are arrays of `int`.

### `block_generic_callback` function

The `block_generic_callback` function is the type of function that should be supplied as actions for events that are tied to a specific class of deck blocks but not a specific instance of a deck block. It is used for initialise and finalise events. In the C interface the type is `deck_block_generic_fn`. It is defined as

```c
void block_generic_callback(char *block_text, int *pass_number,
    int nparents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

* `block_text` is the name of the block that triggered this action. Since this function is attached to a class of deck blocks and not a specific block this will always be the name of a block specified in the definition even if you are remapping a block
* `pass_number` points to the number of the pass through the deck that is currently happening. This is specified by the host code
* `nparents` is the number of elements in `parent_kind`
* `parent_kind` is an array of definition IDs for all of the block definitions in the hierarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

### `block_callback` function

The `block_callback` function is the type of function that should be supplied as actions for events that are related to a specific instance of a block. Currently these are start and end actions. In the C interface the type is `deck_block_fn`. It is defined as

```c
void block_callback(char *block_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

* `block_text` is the name of the block that triggered this action. This is always the name of the block that was actually present in the deck being parsed, even if you are using a remapping function to select the functions to be called for a block
* `pass_number` points to the number of the pass through the deck that is currently happening. This is specified by the host code
* `nparents` is the number of elements in both `parents` and `parent_kind`
* `parents` is an array of instance IDs for all of the blocks in the hierarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the hierarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

### `key_text_callback` function

The `key_text_callback` function is an action function that is called when a key is encountered either in a block or in the root. This action function is the first type that the EIS deck system tries to call for a key and it makes no expectations about the nature of the key beyond it being a section of text. In the C interface the type is `deck_key_text_fn`. The function is defined as

```c
void key_text_callback(char *key_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

* `key_text` is the entire text of the key. Normally in a typical deck this will be of the form "key = value" but this is not enforced
* `pass_number` points to the number of the pass through the deck that is currently happening. This is specified by the host code
* `nparents` is the number of elements in both `parents` and `parent_kind`
* `parents` is an array of instance IDs for all of the blocks in the hierarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the hierarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

Usually you will only want to use `key_text_callback` actions to handle unusual deck keys and use the more specific action functions for normal keys. If you get a key and interpret that it doesn't need to be handled by your `key_text_callback` action function then you should set `*status_code` to `eis_status_not_handled` to indicate that this action function isn't going to handle this key.

If you are certain that the key will not be handled by any later functions then you can set `*errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

### `key_value_callback` function

The `key_value_callback` function is an action function that is called when a key is encountered either in a block or in the root. This action function is considered second by the EIS deck system after `key_text_callback`. It only expects that a key is in either the form `key = value` or `key : value`. In the C interface the type is `deck_key_value_fn`.

```c
void key_value_callback(char *key_text, char *value_text, int *pass_number,
    int nparents, int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

* `key_text` is the part of the key string before the `=` or `:`
* `value_text` is the part of the key string after the `=` or `:`
* `pass_number` points to the number of the pass through the deck that is currently happening. This is specified by the host code
* `nparents` is the number of elements in both `parents` and `parent_kind`
* `parents` is an array of instance IDs for all of the blocks in the hierarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the hierarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

You can test for whether the key contained `=` or `:` by testing for `(*status_code & eis_status_directive) != 0`.

If you get a key and interpret that it doesn't need to be handled by your `key_value_callback` action function then you should set `*status_code` to `eis_status_not_handled` to indicate that this action function isn't going to handle this key.

If you are certain that the key will not be handled by any later functions then you can set `*errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

### `key_numeric_value_callback`

The `key_numeric_value_callback` function is an action function that is called when a key is encountered either in a block or in the root. This action function is considered third by the EIS deck system after `key_value_callback`. It expects that the key is of the form `key = value` or `key : value` and that the value can be parsed into one more numbers (separated by commas if multiple numbers) using the parser specified when the deck definition was created. In the C interface the type is `deck_key_numeric_value_fn`.

```c
void key_numeric_value_callback(char *key_text, int nvalues, EIS_NUM *values,
    int *pass_number, EIS_BITMASK cap_bits, int parser, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

* `key_text` is the part of the key string before the `=` or `:`
* `nvalues` is the number of values in `values`
* `values` is an array of values returned by the parser after the value is converted to a number
* `pass_number` points to the number of the pass through the deck that is currently happening. This is specified by the host code
* `cap_bits` is the capability bits of the parsed value
* `parser` is the handle of the parser that parsed the values
* `nparents` is the number of elements in both `parents` and `parent_kind`
* `parents` is an array of instance IDs for all of the blocks in the hierarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the hierarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

You can test for whether the key contained `=` or `:` by testing for `(*status_code & eis_status_directive) != 0`.

If you get a key and interpret that it doesn't need to be handled by your `key_numeric_value_callback` action function then you should set `*status_code` to `eis_status_not_handled` to indicate that this action function isn't going to handle this key.

If you are certain that the key will not be handled by any later functions then you can set `*errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

### `key_stack_callback`

The `key_stack_callback` function is an action function that is called when a key is encountered either in a block or in the root. This action function is considered last by the EIS deck system after `key_numeric_value_callback`. It expects that the key is of the form `key = value` or `key : value` and that the value can be parsed into a stack using the parser specified when the deck definition was created (or a default parser if no parser was specified). In the C interface the type is `deck_key_stack_fn`.

```c
void key_stack_callback(char *key_text, int value_stack, int *pass_number,
    int parser, int nparents, int *parents, int *parent_kind,
    EIS_STATUS *status_code, EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

* `key_text` is the part of the key string before the `=` or `:`
* `value_stack` is the handle of a stack that was tokenized by the parser. It is a copy that belongs to your code. Release it with `eis_stack_dec_ref` when you have finished with it, or set `*status_code` to include `eis_status_delete_stack` to have EIS release it when the function returns
* `pass_number` points to the number of the pass through the deck that is currently happening. This is specified by the host code
* `parser` is the handle of the parser that tokenized the stack
* `nparents` is the number of elements in both `parents` and `parent_kind`
* `parents` is an array of instance IDs for all of the blocks in the hierarchy up to and including the current block
* `parent_kind` is an array of definition IDs for all of the block definitions in the hierarchy up to and including the definition for the current block
* `status_code` is used to pass status information from the EIS deck system to the function and return data from the function that is not an error.
* `host_state` is used to pass back a bitmask to the host code containing information from the callback function
* `errcode` is used to pass error information to the EIS deck system

You can test for whether the key contained `=` or `:` by testing for `(*status_code & eis_status_directive) != 0`.

Although the `key_stack_callback` function is the last action function to be considered for a key it is still valid to set `*status_code` to `eis_status_not_handled`. This will cause the fallback `any_key` functions to be called if appropriate or the key to be marked as not recognized if no handler could be found.

If you are certain that the key will not be handled by any later functions then you can set `*errcode` to `eis_err_unknown_key` and no further options will be considered for this key.

## Defining a single level deck

The simplest form of deck is the single level or EPOCH style deck. This type of deck consists of all keys being contained in blocks and only one level of block

To define a deck you have to create a deck definition by calling `eis_deck_create_definition`, which returns the handle of the definition. This function has quite a few optional parameters. The only one that you will usually want for a single level deck is `interop_parser`, which is the handle of a maths parser that will be used to parse deck text to a stack or number if requested. If you do not specify a parser then a default parser is created as needed but this will only be able to parse the basic EIS maths expressions. Next you get the ID of the root block, which is what blocks and keys are added to, by calling `eis_deck_get_root_block` with the handle of the definition.

Once you have the ID of the root block you will want to call `eis_deck_add_block` to add the specific blocks that will appear in your deck. It takes the handle of the definition, the ID of the block to add the new block to, which is the root block here, and `block_name`, which is the name of the block to add. It returns the ID of the new block. The block will be referred to by the name that you give it, and it will be triggered by `begin:block_name` in the deck file when you parse it. `eis_deck_add_block` also has a lot of optional parameters but the ones that would typically be used for a simple deck are

* `init_block` - a `block_generic_callback` function that is called the first time that "start:block\_name" is encountered for this block. It is not called on subsequent "start:block\_name" appearances
* `start_block` - a `block_callback` function that is called each time "start:block\_name" is encountered for this block
* `end_block` - a `block_callback` function that is called each time "end:block\_name" is encountered for this block
* `final_block` - a `block_generic_callback` function that is called when the deck parsing finished for every block that appears at least once in the deck

In fact we can create a simple code using just these pieces, although they are not very useful. This example code does include an actual parse of a deck file that we will cover later.

### Adding blocks

```c
#include <stdio.h>
#include "eis_deck.h"

void init_block(char *block_text, int *pass_number, int nparents,
    int *parent_kind, EIS_STATUS *status_code, EIS_BITMASK *host_state,
    EIS_ERROR *errcode)
{
  printf("Calling init for block : %s\n", block_text);
}

void final_block(char *block_text, int *pass_number, int nparents,
    int *parent_kind, EIS_STATUS *status_code, EIS_BITMASK *host_state,
    EIS_ERROR *errcode)
{
  printf("Calling final for block : %s\n", block_text);
}

void start_block(char *block_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode)
{
  printf("Calling start for block : %s\n", block_text);
}

void end_block(char *block_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode)
{
  printf("Calling end for block : %s\n", block_text);
}

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_block_params block_params;
  eis_deck_parse_file_params parse_params;

  printf("This example works with the demo1.deck file to demonstrate parsing ");
  printf("a simple deck file. It defines two blocks \"block1\" and ");
  printf("\"block2\" that *may* be present in a deck. Neither has any keys ");
  printf("defined so specifying any keys or any other blocks will raise an ");
  printf("error\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set the action functions*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);
  block = eis_deck_add_block(dfn, root, "block2", &errcode, &block_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  /*An optional parameter is used here to allow empty blocks*/
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo1.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }

  /*These are not necessary but this is how you release a deck parser and a
  deck definition when you are done with them*/
  eis_deck_parser_dec_ref(deck, NULL);
  eis_deck_definition_dec_ref(dfn, NULL);
}
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

Each block can have keys in it by adding a key to it. You do this by using the ID of the block that you get from calls to `eis_deck_add_block` and calling `eis_deck_add_key`. It takes the handle of the definition, the ID of the block, a name and optionally one or more key handling functions. The key handling functions are called

1. `key_text_fn` - A `key_text_callback` function. This is tried first and simply passes the entire line in the deck to the function for the host code to handle
2. `key_value_fn` - A `key_value_callback` function. This is tried second and splits the line on either an `=` or `:` symbol and passes the two parts as strings to the function
3. `key_numeric_value_fn` - A `key_numeric_value_callback` function. This is tried third and splits the line on `=` or `:` but takes the right hand part and evaluates it in a maths parser into one or more numeric values. This is mainly intended for using the parser as a calculator for evaluating mathematical expressions into simple values. The expression is evaluated with parser host parameters set to `NULL`.
4. `key_stack_fn` - A `key_stack_callback` function. This is tried last and splits the line on `=` or `:` but takes the right hand part and converts it into a stack that can be evaluated or stored as required by the host code.

`key_text_fn` is intended for very specific types of deck so we recommend only using 2-4. Option 2 which involves you being handed the text for the key and value parts of the expression is the most general. From that part you can parse the text as you want including running it through the maths parser, but also use values to open files etc. etc.

A very simple example is given below. This reuses the block init, final, start and end calls from the previous examples but adds a function that is called when specific keys are present in the blocks. The keys `key1` and `key2` are added to `block1` and the key `new_key` is added to `block2`

```c
/*init_block, final_block, start_block and end_block are the same as in the
previous example*/

void key_sub(char *key_text, char *value_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode)
{
  int i;

  printf(" Found in block :");
  for (i = 0; i < nparents; ++i) printf(" %d", parents[i]);
  printf("\n Found in block of type :");
  for (i = 0; i < nparents; ++i) printf(" %d", parent_kind[i]);
  printf("\n Found known key : %s with value %s\n", key_text, value_text);
}

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This extends demo1 by adding keys to the blocks. \"block1\" now ");
  printf("has keys \"key1\" and \"key2\" and \"block2\" has the key ");
  printf("\"new_key\". When any or all of these keys are encountered they ");
  printf("are printed. If any other keys are encountered an error occurs\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set the action functions*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;

  /*An optional parameter is used here to set the key function*/
  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_sub;

  block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);
  eis_deck_add_key(dfn, block, "key1", &errcode, &key_params);
  eis_deck_add_key(dfn, block, "key2", &errcode, &key_params);

  block = eis_deck_add_block(dfn, root, "block2", &errcode, &block_params);
  eis_deck_add_key(dfn, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo2.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
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
 Found in block : 0 1
 Found in block of type : 0 1
 Found known key : key2 with value mykey
Calling end for block : block1
Calling init for block : block2
Calling start for block : block2
 Found in block : 0 2
 Found in block of type : 0 2
 Found known key : new_key with value Hello!
Calling end for block : block2
Calling start for block : block1
 Found in block : 0 3
 Found in block of type : 0 1
 Found known key : key1 with value test_key
Calling end for block : block1
Calling final for block : block1
Calling final for block : block2
```

You will notice that in this case the same `key_value_callback` function is used for all three keys. This is completely valid and the function gives you enough information to know both which specific block and which kind of block the key was found in. Typically this isn't used much in working code where each key would be bound to a single function that triggers for that key but sometimes there are several keys that have similar enough behaviour that this would cause a lot of duplicated code so this is supported.

Each block definition is given a unique ID when it is added and an array of the IDs for all block definitions leading to the definition of the current block is available in all block init, final, start and end functions and in all key functions. It is in the array `parent_kind`. 0 is the special value for the definition of the root block (outside all named blocks). All blocks have the root block as a parent so the `parent_kind` array always has at least two elements for all named blocks (it is 1 element for keys in the root). The parent kind is printed in this example in the lines reading `Found in block of type`. These show that the first parent is always 0 (the root) but then for blocks of type `block1` the next value is always `1` and for blocks of type `block2` the second value is always `2`. You can retrieve the name of a block from the definition ID using `eis_deck_get_block_name`, which takes the handle of the definition, the ID of the block, the size of your buffer and the buffer, and returns the number of characters needed so that you can pass a `NULL` buffer to find out the size.

Each instance of a block in a deck is also given a unique ID as well and this ID is not related to the ID of the _definition_ for this type of block. These IDs are available in block start and end functions and in key functions. They are not available in block init and block final functions because these are connected to definitions and not specific blocks. They are in the `parents` array. 0 is the special value for the instance of the root block in the deck and all blocks are children of the root instance so all `parents` arrays have at least 2 elements for named blocks (it has one element for keys in the root). In the example above the `parents` array is printed in the lines reading `Found in block`. You can see here that even blocks having the same _definition_ ID have different _block_ IDs. So the first instance of `block1` has a `parents` ID of 1 and a `parent_kind` ID of 1. This is only a coincidence and the IDs are *not* related. The second instance of `block1` still has a `parent_kind` ID of 1 but the `parents` ID is now 3. You can retrieve the name of a block from the unique block ID using `eis_deck_get_parsed_block_name`, which takes the handle of the deck parser in place of the definition but is otherwise used in the same way, but many codes will want to record this ID in some other way, especially if you are using a hierarchical deck. NOTE : The ID for each block is assigned depending on that block's position in the deck file. The same block appearing at a different point in the deck will have a different ID. If you want to uniquely identify a block so that it can always be identified regardless of its position in the deck file you should set up a definition where the user gives the block a unique name key that the host code should track.

### Numerical keys

The next most common thing to want to do is to parse the keys to numerical values. This is done using a `key_numeric_value_callback` function passed as the optional parameter `key_numeric_value_fn` to `eis_deck_add_key`. To demonstrate that the parser has converted the text to a number this example both prints the value and the value - 1

```c
#include <stdio.h>
#include "eis_deck.h"

/*init_block, final_block, start_block and end_block are the same as in the
first example*/

void key_sub(char *key_text, int nvalues, EIS_NUM *values, int *pass_number,
    EIS_BITMASK cap_bits, int parser, int nparents, int *parents,
    int *parent_kind, EIS_STATUS *status_code, EIS_BITMASK *host_state,
    EIS_ERROR *errcode)
{
  printf("Found key %s. It has %d elements\n", key_text, nvalues);
  printf("Value of first result is %g\n", values[0]);
  printf("Value of first result -1 is %g\n", values[0] - 1.0);
}

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This extends demo2 with keys that have numerical values. So long ");
  printf("as the keys have values that are evaluable by the EIS parser they ");
  printf("will be evaluated and the result printed. Unknown keys or keys ");
  printf("that cannot be evaluated by the EIS parser will cause an error\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set the action functions*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;

  /*An optional parameter is used here to set the key function*/
  eis_deck_add_key_params_init(&key_params);
  key_params.key_numeric_value_fn = key_sub;

  block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);
  eis_deck_add_key(dfn, block, "key1", &errcode, &key_params);
  eis_deck_add_key(dfn, block, "key2", &errcode, &key_params);

  block = eis_deck_add_block(dfn, root, "block2", &errcode, &block_params);
  eis_deck_add_key(dfn, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo3.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
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

A common thing to want to do is parse some kinds of values as strings and some as numbers. Normally the text parser runs before the numeric parser so you have to use some kind of mechanism to detect which keys should be handled as text. There are a lot of ways to do this but for this example we're just going to enclose strings in quotes.

```c
#include <stdio.h>
#include <string.h>
#include "eis_deck.h"

void key_str_sub(char *key_text, char *value_text, int *pass_number,
    int nparents, int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode)
{
  /*If no quotes are present then this is not a bad value*/
  char *lq = strchr(value_text, '"');
  char *uq;
  if (lq == NULL) {
    *status_code = eis_status_not_handled;
    return;
  }

  uq = strrchr(value_text, '"');
  /*Otherwise check that there are two quotes and that there is at least
  a single character between them. If not then this is a bad value*/
  if (uq - lq < 2) {
    *errcode = eis_err_bad_value;
    return;
  }

  printf("Found text key %s. Value is %.*s\n", key_text, (int)(uq - lq - 1),
      lq + 1);
}

void key_val_sub(char *key_text, int nvalues, EIS_NUM *values,
    int *pass_number, EIS_BITMASK cap_bits, int parser, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode)
{
  int i;

  printf("Found numerical key %s. Values are", key_text);
  for (i = 0; i < nvalues; ++i) printf(" %g", values[i]);
  printf("\n");
}

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This is an extension of demo3. In this example keys can be ");
  printf("handled as either text (if they are enclosed in double quotes) ");
  printf("or as numbers if they are not in quotes.\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set two key functions*/
  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_str_sub;
  key_params.key_numeric_value_fn = key_val_sub;

  block = eis_deck_add_block(dfn, root, "block1", &errcode, NULL);
  eis_deck_add_key(dfn, block, "key1", &errcode, &key_params);
  eis_deck_add_key(dfn, block, "key2", &errcode, &key_params);

  block = eis_deck_add_block(dfn, root, "block2", &errcode, NULL);
  eis_deck_add_key(dfn, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo4.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
```

The function `key_str_sub` checks for the first and last instances of `"` in a key. It then checks if either there are either

1. No `"` characters in the value string. In this case it sets `*status_code` to `eis_status_not_handled` because this flags that it should try to parse the value as a maths parser expression

2. Only one `"` in the value string or two that have no space between them. In this case it sets `*errcode` to `eis_err_bad_value` because this is a string that it should try to treat as a string but wasn't able to validly

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
Found numerical key new_key. Values are 19
Found numerical key key1. Values are 0.866025
```

For keys where text parsing isn't possible then simply don't specify the optional parameter `key_value_fn` when creating the key and the parser will only try to parse that key to values. Similarly if you don't specify `key_numeric_value_fn` then only the text parsing will occur.

### Text fallback parsing

An alternative to the above behaviour is "text fallback" processing. In this the value is evaluated first to a value (or stack) and if this fails the text functions are called. To set this behaviour set the optional parameter `text_fallback` to 1 for `eis_deck_add_key`. To set this behaviour for all keys in a block (unless overridden for a specific key) set the same optional parameter for `eis_deck_add_block`. To set this behaviour for all keys in a definition (unless overridden for a block or key) set the same optional parameter for `eis_deck_create_definition`

### Stack keys

The more general version of numerical keys are stack keys. This mode of parsing will be tried last, after all other modes are rejected either by not having an action function associated with them or if they all return a status of `eis_status_not_handled`. This mode is very similar to parsing keys to numeric values but the action function is called with a stack rather than the numerical values. This is more flexible in various ways.

1. You can pass host parameters when parsing the expression
2. You can evaluate the stack multiple times (often with different host parameters)
3. You can copy the stack so that you can keep it and evaluate it in the future if wanted

### "Any" keys

As well as having specific action functions for known keys, EIS deck also offers the options of having action functions that trigger for any keys that either don't have specific action functions associated with them or where all of the specific action functions return the `eis_status_not_handled` status.

You specify "Any" action functions when you create a block in `eis_deck_add_block`. You set the optional parameters

1. `any_key_text` - any version of `key_text_fn`
2. `any_key_value` - any version of `key_value_fn`
3. `any_key_numeric_value` - any version of `key_numeric_value_fn`
4. `any_key_stack` - any version of `key_stack_fn`

"Any" functions have the same signature as the normal versions and behave in the same way, but you are essentially required to check the `key_text` parameters to make sure that you are handling a key that makes sense.

```c
/*key_str_sub and key_val_sub are the same as in the mixed keys example*/

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This example shows the use of \"any_key\" functions. These ");
  printf("optional functions are called when a key is encountered that ");
  printf("hasn't been explicitly added to the block. \"block1\" will accept ");
  printf("any key but \"block2\" still has only the fixed key \"new_key\"\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set the "any" functions*/
  eis_deck_add_block_params_init(&block_params);
  block_params.any_key_value = key_str_sub;
  block_params.any_key_numeric_value = key_val_sub;
  block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);

  block = eis_deck_add_block(dfn, root, "block2", &errcode, NULL);

  /*Optional parameters are used here to set two key functions*/
  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_str_sub;
  key_params.key_numeric_value_fn = key_val_sub;
  eis_deck_add_key(dfn, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo5.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
```

In this example `block1` uses any key functions while `block2` still uses specific named keys. Block 1 will now accept any keys in the block rather than simple named keys.

If `any` functions are used then the error that is reported when a key fails to be recognized by all of the `any` functions depends on whether or not the key has a specific action function for it too. If a key has no specific action functions associated with it then when the last `any` function fails an `eis_err_unknown_key` error occurs. If a key has a specific action function associated with it then when the last any function fails an `eis_err_bad_key` error occurs.

### Defining a multi level deck definition

Decks that have blocks defined within blocks are called multi-level decks. Multi level decks are specified in the same way as single level decks, but you call `eis_deck_add_block` with the ID of a block rather than the ID of the root block as the block to add to. The function is exactly the same and takes all of the same parameters.

```c
/*key_str_sub and key_val_sub are the same as in the mixed keys example*/

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This example shows how to create subblocks. \"block2\" is made a ");
  printf("sub block of \"block1\". It behaves exactly the same as ");
  printf("\"block2\" in the previous examples but \"block2\" should now be ");
  printf("defined inside \"block1\". If you put \"block2\" outside ");
  printf("\"block1\" it is now an invalid block\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set the "any" functions*/
  eis_deck_add_block_params_init(&block_params);
  block_params.any_key_value = key_str_sub;
  block_params.any_key_numeric_value = key_val_sub;
  block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);

  /*block2 is added to block1 rather than to the root*/
  block = eis_deck_add_block(dfn, block, "block2", &errcode, NULL);

  /*Optional parameters are used here to set two key functions*/
  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_str_sub;
  key_params.key_numeric_value_fn = key_val_sub;
  eis_deck_add_key(dfn, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo6.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
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

because block2 is only a valid sub-block of block1. If you wanted block2 to be in both block1 and the root block then you would have to add a block called "block2" to both block1 and the root block with the same action functions. It is important to note that these blocks will still be distinct and have different unique `parent_kind` definition ID numbers but the host code can ignore this fact and make both blocks behave identically if wanted.

In multi level decks there is a strict parsing order.

1. All keys at each level are parsed first in the order that they appear in the deck
2. All blocks are then parsed in the order they appear in the deck
3. If a block contains sub-blocks then all of its sub-blocks are parsed to full depth before the next block is parsed

### Block remapping

Because blocks have a strict hierarchy that is described through their unique ID keys there is no direct equivalent to `any` action functions for blocks. The equivalent for blocks is called `block remapping` and works by taking a block with an unknown name and stating what block type it should be treated as by specifying the name of the block. Block remapping is a block by block level process with each block (including the root block) having an optional remapping function that is used if an unknown block is encountered in that block. You might want to use block remapping if you want a user to be able to use the name of something they specified in the name of a block. For example

```
begin:object
  object_name = my_object
end:object

begin:output_my_object

end:output_my_object
```

as a way of specifying output options for a named object. You could implement this in other ways in an EIS deck (either have an output block within the object block or have an object_name key in the output block to tie the block to the key) but this allows a developer to use user specified text in input deck block names.

Block remapping functions have the following signature. In the C interface the type is `deck_block_remap_fn`

```c
void block_remap_callback(char *block_text, int *pass_number,
    int len_remap_text, char *remap_text, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

The `block_text` argument will contain the block name found in the deck file. `remap_text` is an empty string with room for `len_remap_text` characters, including the terminating null, which is large enough to contain the largest possible block that you could remap to. You should fill it with the name of the block that you should treat this block as. If you cannot remap the block name then you should set `*status_code` to `eis_status_not_handled`.

You set the remapping function by specifying the optional parameter `block_remapper` as a function with the signature of a `block_remap_callback` to either `eis_deck_add_block` (describing a block) or `eis_deck_create_definition` where it is applied to the root block.

```c
#include <stdio.h>
#include <string.h>
#include "eis_deck.h"

int dfn, deck;

void remapper(char *block_text, int *pass_number, int len_remap_text,
    char *remap_text, EIS_STATUS *status_code, EIS_BITMASK *host_state,
    EIS_ERROR *errcode)
{
  if (strstr(block_text, "block") != NULL) {
    snprintf(remap_text, len_remap_text, "generic_block");
  } else {
    *status_code = eis_status_not_handled;
  }
}

/*init_block, final_block, start_block and end_block are the same as in the
first example*/

void key_sub(char *key_text, char *value_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode)
{
  char name[1000];

  printf("Found key %s with value %s\n", key_text, value_text);
  eis_deck_get_block_name(dfn, parent_kind[nparents - 1], 1000, name, NULL);
  printf("Block generic name for key is %s\n", name);
  eis_deck_get_parsed_block_name(deck, parents[nparents - 1], 1000, name,
      NULL);
  printf("Block specific name for key is %s\n", name);
}

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int root, block, ierr;
  char str[10000];
  eis_deck_create_definition_params dfn_params;
  eis_deck_add_block_params block_params;
  eis_deck_parse_file_params parse_params;

  printf("This example demonstrates block remapping. Block remapping allows ");
  printf("an EIS host code to tell EIS to look up a block under a name other ");
  printf("than the name that actually appears in the deck. In this case it ");
  printf("involves remapping any block containing the word \"block\" to the ");
  printf("block \"generic_block\"\n");

  /*An optional parameter is used here to set the remapper for the root*/
  eis_deck_create_definition_params_init(&dfn_params);
  dfn_params.block_remapper = remapper;
  dfn = eis_deck_create_definition(&errcode, &dfn_params);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  /*Optional parameters are used here to set the action functions*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block_params.any_key_value = key_sub;
  block = eis_deck_add_block(dfn, root, "generic_block", &errcode,
      &block_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  eis_deck_parse_file(deck, dfn, "demo7.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
```

In this example any block beginning with the word `block` is remapped to use the action functions associated with the `generic_block` block, so the test deck

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
Found key key2 with value "my key"
Block generic name for key is generic_block
Block specific name for key is block_1
Calling end for block : block_1
Calling start for block : block_2
Found key new_key with value 7+12
Block generic name for key is generic_block
Block specific name for key is block_2
Calling end for block : block_2
Calling start for block : block_1
Found key key1 with value sin(pi/3)
Block generic name for key is generic_block
Block specific name for key is block_1
Calling end for block : block_1
Calling final for block : generic_block
```

As you can see from that output when doing block remapping there is a slight change to the behaviour of the init/start/end/final action functions. The init and final action functions are not associated with any particular block so they are called with a `block_text` parameter of the generic block name that is being remapped to. The start and end action functions are specific to a given block so they are passed the text of the block that is actually being encountered in the deck without name remapping. This is how you write the code to be able to deal with different blocks having different behaviours. Since you will often not want the generic block name to be a valid block in the deck you would want to test for the generic name being passed as the `block_text` parameter to the `start` action function and return `eis_err_unknown_block` error if it is found.

### Pass triggering
When you call to parse a deck you can specify the `pass_number`. This is used to specify that you intend to run through your deck multiple times, presumably doing different things each time. It is associated with another concept `max_passes` which says the number of times that you intend to run through your deck. Block initialisation only happens on pass number 1 and block finalisation only automatically on pass number `max_passes`, but you can also specify certain blocks or keys should only be processed on certain pass numbers. When you create a block or a key you can optionally set one of the following integer optional parameters which will restrict which passes a key or block should trigger on.

1. pass_eq - Block should trigger on this pass number
2. pass_le - Block should trigger if pass number is less than or equal to the parameter
3. pass_ge - Block should trigger if pass number is greater than or equal to the parameter

If you specify multiple of these parameters then they are combined as OR, so `pass_le = 3, pass_ge = 5` would mean "trigger for all passes except pass 4". You cannot at present specify more complex conditions and should use the `pass_number` parameter to the callback functions and have the function triggered for all passes if you want more complex behaviour.

On a pass which is flagged to not trigger a key they key is not processed. On a pass which is flagged to not trigger a block then block start, init and end behaviours will not trigger. Finalise will occur on the final pass regardless of the block being marked to trigger on the final pass or not. The behaviour is hierarchical by default, so all keys in a block and all subblocks of a block inherit its pass triggering behaviour by default. Manually specifying a behaviour will override the behaviour from the parent. Thus, a block which is not triggered on a particular pass may have individual sub-blocks or keys that will trigger, but it will *not* trigger any `any key` behaviour for that block. Block remapping will occur under all circumstances and the pass triggering behaviour of the remapped block is the behaviour of the block that it is remapped to. An example of pass triggering is

```c
/*init_block, final_block, start_block and end_block are the same as in the
first example. key_sub is the same as in the keys example*/

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr;
  char str[10000];
  eis_deck_add_block_params block_params;
  eis_deck_add_key_params key_params;
  eis_deck_parse_file_params parse_params;

  printf("This demonstrates multiple passes through a deck. The block ");
  printf("\"block1\" is only parsed on the first pass through the deck. ");
  printf("\"block2\" is only parsed on the second pass through\n");

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);

  eis_deck_add_key_params_init(&key_params);
  key_params.key_value_fn = key_sub;

  /*Optional parameters are used here to set the action functions and the
  pass number that the block triggers on*/
  eis_deck_add_block_params_init(&block_params);
  block_params.init_block = init_block;
  block_params.start_block = start_block;
  block_params.end_block = end_block;
  block_params.final_block = final_block;
  block_params.pass_eq = 1;
  block = eis_deck_add_block(dfn, root, "block1", &errcode, &block_params);
  eis_deck_add_key(dfn, block, "key1", &errcode, &key_params);
  eis_deck_add_key(dfn, block, "key2", &errcode, &key_params);

  block_params.pass_eq = 2;
  block = eis_deck_add_block(dfn, root, "block2", &errcode, &block_params);
  eis_deck_add_key(dfn, block, "new_key", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);

  /*Optional parameters are used here to say which pass this is and how
  many passes there will be*/
  eis_deck_parse_file_params_init(&parse_params);
  parse_params.allow_empty_blocks = 1;
  parse_params.max_passes = 2;

  printf("*** First pass through deck ***\n");
  parse_params.pass_number = 1;
  eis_deck_parse_file(deck, dfn, "demo8.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }

  printf("*** Second pass through deck ***\n");
  parse_params.pass_number = 2;
  eis_deck_parse_file(deck, dfn, "demo8.deck", &errcode, &parse_params);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
  }
}
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

The final way in which EIS can report key values from a deck is straight into a numerical variable. This requires that a key be of key/value or directive form. The output can be placed into a variable of the following types : 32 bit integer, 64 bit integer, 32 bit real, 64 bit real or any array of those types. When the key is encountered it is automatically converted into a numerical representation and placed into the associated variable. Assignment to a scalar variable will only succeed if the key evaluates to a single value and assignment to an array will only succeed if the array is at least large enough to hold all of the returned values. If the array is larger than the number of returned values then the values are populated from lowest to highest. If assignment variables are combined with action functions then they are considered after all action functions if no action function has handled the key but before any `any` functions are triggered.

To return a value from a key to an assignment variable you set one of the following optional parameters of `eis_deck_add_key`, setting the value to a pointer to the variable to hold the result for the key. Each has a matching length parameter, which is 1 by default for a scalar variable. For an array set the length parameter to the number of elements in the array

1. i32value, i32len - Assign return value to the specified 32 bit integer scalar or array
2. i64value, i64len - Assign return value to the specified 64 bit integer scalar or array
3. r32value, r32len - Assign return value to the specified 32 bit real scalar or array
4. r64value, r64len - Assign return value to the specified 64 bit real scalar or array

The following example code shows a simple use of assignment variables

```c
#include <stdio.h>
#include <stdint.h>
#include "eis_deck.h"

int main(int argc, char** argv)
{
  EIS_ERROR errcode;
  int dfn, deck, root, block, ierr, i;
  char str[10000];
  int32_t scalar_value = 0;
  int32_t array_value[4] = {0, 0, 0, 0};
  eis_deck_add_key_params key_params;

  dfn = eis_deck_create_definition(&errcode, NULL);
  root = eis_deck_get_root_block(dfn, &errcode, NULL);
  block = eis_deck_add_block(dfn, root, "block1", &errcode, NULL);

  /*An optional parameter is used here to set the assignment variable*/
  eis_deck_add_key_params_init(&key_params);
  key_params.i32value = &scalar_value;
  eis_deck_add_key(dfn, block, "scalar_value", &errcode, &key_params);

  /*Optional parameters are used here to set an assignment array*/
  eis_deck_add_key_params_init(&key_params);
  key_params.i32value = array_value;
  key_params.i32len = 4;
  eis_deck_add_key(dfn, block, "array_value", &errcode, &key_params);

  deck = eis_deck_create_parser(&errcode, NULL);
  eis_deck_parse_file(deck, dfn, "demo9.deck", &errcode, NULL);
  if (errcode != eis_err_none) {
    for (ierr = 1; ierr <= eis_deck_get_error_count(deck, NULL); ++ierr) {
      eis_deck_get_error_report(deck, ierr, 10000, str, NULL);
      printf("%s\n", str);
    }
    return 1;
  }

  printf("scalar_value is %d\n", scalar_value);
  for (i = 0; i < 4; ++i) printf("array_value[%d] is %d\n", i, array_value[i]);
}
```

and associated deck

```
begin:block1
  scalar_value = 1
  array_value = 1,2,3,4
end:block1
```

Assignment variables are very powerful but come with a variety of problems. If a variable that an assignment key refers to goes out of scope or is freed then the deck key associated with it must be recreated as well or the code will crash when that key is encountered. Also assuming that you are reading variables from the deck for use as your code runs performance of your core code may well be lower using pointer variables than using a normal variable and using an action function to set its values.

### Event functions

You can bind event functions to a deck definition that will trigger when an event occurs. The event functions are different to action functions because they are triggered by a *type* of action occurring rather than being connected to a specific named block or key. You bind event functions with the optional parameters of `eis_deck_create_definition`. There are 7 event functions

1. `on_key_success` which occurs when a key is successfully parsed
2. `on_key_failure` which occurs when a key has not been able to be parsed, either due to the key not being known (and not being handled by an `any_key` function) or due to a failure in key handling
3. `on_key_no_trigger` which occurs when a key has not been triggered because of pass triggering rules
4. `on_block_start` which occurs when a block starts. It occurs for both non-remapped and remapped blocks
5. `on_block_end` which occurs when a block ends. It occurs for both non-remapped and remapped blocks
6. `on_block_no_trigger` which occurs when a block would start but does not because of pass triggering rules. There is no matching untriggered block end
7. `on_block_failure` which occurs when an error occurred when starting or ending a block. Usually this is because a block was not found but this event will trigger if any error is reported during a block start or a block end

All of the event functions are of the same type. In the C interface the type is `deck_event_fn`

```c
void event_callback(char *event_text, int *pass_number, int nparents,
    int *parents, int *parent_kind, EIS_STATUS *status_code,
    EIS_BITMASK *host_state, EIS_ERROR *errcode);
```

The data that is passed to the event functions is mostly always the same, but there are a few differences. The `event_text` parameter contains the name of the block that caused the event for block functions and the entire text (key and value) for key functions. The `parents` and `parent_kind` parameters contain the same information as the equivalent parameters for the action functions _except_ for when the `on_block_failure` event is triggered because of an unknown block in which case `nparents` is 0. The `errcode` parameter will usually be `eis_err_none` but in the case of `on_block_failure` and `on_key_failure` events it will be populated with the error code that caused the failure. Overriding this error code will replace the error originally reported. In particular setting it to `eis_err_none` will prevent the error from triggering.

Event functions are intended for a variety of purposes but the main purpose is to allow for simple reporting of keys and blocks in a deck. You might, for example, bind event functions to `on_block_start`, `on_block_end`, `on_key_success` and `on_key_failure` to simply print the blocks and keys encountered into a file to help with reporting status if a user presents a malformed deck.

### Descriptions

You can optionally specify a description of the meaning and function of a block or key when you add the block or key to the definition. This is done by setting the optional parameter `description` of `eis_deck_add_block` or `eis_deck_add_key`. The descriptions are mainly retrieved through `eis_deck_markdown_definition`. This function takes the handle of the definition, the size of your buffer and the buffer, and returns the number of characters needed so that you can pass a `NULL` buffer to find out the size. It returns a common markdown document describing all of the blocks in the definition

## Loading and parsing a deck

Once you have a deck definition created you will want to actually use it to parse a deck. The previous examples have shown how to do this - by creating a deck parser and using it to read a text file. The function that you will usually want to use is `eis_deck_parse_file`. This takes 4 compulsory arguments

1. The handle of the deck parser
2. The handle of the deck definition to parse the deck using. This definition *must* have already been set up
3. String containing name of the deck file to load
4. A pointer to an `EIS_ERROR` to hold any error codes from the parse. If this is not `eis_err_none` then you should check the error report

It also has 9 optional parameters. An optional parameter that is a flag is 1 for true and 0 for false, and -1 means that it is not specified and the setting that the deck parser was created with is used

1. `allow_root_keys` - Flag for whether keys in the root level (outside all blocks) should be permitted. Default false
2. `allow_empty_blocks` - Flag for whether blocks that contain no keys is permitted or not. Default false
3. `unknown_block_is_fatal` - Flag for whether to finish parsing when an unknown block is encountered. Default true
4. `unknown_key_is_fatal` - Flag for whether to finish parsing when an unknown key is encountered. Default true
5. `initialise_all_blocks` - Flag for whether to initialise all blocks that are known by the parser at the start of the parse rather than when the first block is encountered. Default false and blocks are initialised as they are encountered in the deck.
6. `max_level` - The maximum depth of blocks in the deck parser. If 0 then only keys in the root block are permitted, if 1 then only one level of blocks are permitted, if 2 then those blocks can have sub-blocks, but the sub-blocks may not themselves have sub-blocks, etc. etc. If not specified (-1) then blocks as deep as they are defined in the definition are permitted
7. `pass_number` - Allows multiple passes of a deck and should be used with the `max_passes` optional parameter. If `pass_number` is set to `1` then blocks will be initialised as they are encountered (or as requested in `initialise_all_blocks`). If `pass_number` is equal to `max_passes` then blocks that were encountered will be finalised at the end of the call. If they are not equal then no finalisation will be performed. This is aimed at dealing with host codes that need to deal with parsing the deck multiple times, usually because information is needed to set up arrays or other data structures before they are populated. For host codes that may have an uncertain number of passes through a deck file you should set `max_passes` to a number large enough for the maximum number of passes and call `eis_deck_finalise` when parsing is complete. Both default to 1.
8. `max_passes` - Specifies the maximum number of times that the deck will be parsed. Used in conjunction with `pass_number`. If `pass_number` equals `max_passes` then deck blocks will be finalised when this call to the function finishes, otherwise they will not be. This is aimed at dealing with host codes that need to deal with parsing the deck multiple times, usually because information is needed to set up arrays or other data structures before they are populated. For host codes that may have an uncertain number of passes through a deck file you should set `max_passes` to a number large enough for the maximum number of passes and call `eis_deck_finalise` when parsing is complete.
9. `state` - A pointer to an arbitrary `EIS_BITMASK`. It's intended to allow the host code a simple way of flagging internal states. The value that it holds is passed in to all of the action functions for both blocks and keys, as `host_state`, and may be modified there, and the final value is written back to it when the call finishes. It is "intended" that this will be a state bitmask (hence the name of the type) but it could be used to refer to an item in an array or similar mechanism. Obviously it is also possible for a host code to use a mechanism not connected to EIS to report errors and or statuses.

The `eis_deck_parse_file` function loads the specified file, deals with whitespace, comment characters and imported files and then splits into blocks and keys and compares those blocks and keys with the definition and calls the appropriate action functions.

The next most common way of handing a deck to EIS is using `eis_deck_parse_string`. This is the same as `eis_deck_parse_file` but takes a string containing an entire input deck rather than a filename. There is also an extra optional parameter

1. `filename` - Optional string saying the name of the file that the string provided to `eis_deck_parse_string` came from. If provided then the error reporting will include the name of this file in the errors but otherwise has no effect.

Finally there is a convenient way of dealing with the case of a distributed code that wants to load a file (including import directives) on one processor and distribute it to other processors while keeping the helpful error reporting etc. This introduces two new functions - one to load the file on one processor in a way that fully describes included files etc. and one to parse this loaded data as a deck.

To load the file you call `eis_deck_get_serialised` usually only on a single processor. This function takes

1. The handle of the deck parser
2. A string holding the filename of the deck file to load
3. The size of your buffer to hold the loaded, serialised data
4. The buffer. Pass `NULL` to find out the size that you need
5. A pointer to an `EIS_ERROR` to hold the error code. If this is not `eis_err_none` after the call then you should test the errors from the parser

The function returns the number of characters in the serialised data, or -1 if it could not be loaded. The result of calling this function (if it succeeds) is to fill the buffer with a version of the file specified on the disk, including enough information to reconstruct information about included files, line numbers etc. The data can contain null characters, so always use the returned length rather than the length of the string when you send it through communication channels.

`eis_deck_get_serialised` *ONLY* produces the serialised version of the deck. It doesn't parse the deck in any way because this would cause problems for codes that need explicit synchronization in their action functions. To parse the deck that you got from `eis_deck_get_serialised` you have to use `eis_deck_parse_serialised`. This is almost identical to `eis_deck_parse_file` and `eis_deck_parse_string` but the argument that is the filename in `eis_deck_parse_file` and the text to parse in `eis_deck_parse_string` should be replaced with the output from `eis_deck_get_serialised` and its length.

## Threads and the deck parser
The deck parser is necessarily stateful. At present it is not thread safe and you must have one deck parser and one deck definition per active thread.

## Error reporting

The deck parser has built in reporting of errors as shown in the example code already given. To find out the number of errors call `eis_deck_get_error_count`. To get the report for each error call `eis_deck_get_error_report`, where the errors are numbered from 1. It takes the handle of the deck parser, the number of the error, the size of your buffer and the buffer, and returns the number of characters needed including the terminating null. This tells you the location of all of the errors that were encountered in both the deck parsing and any maths parsing of keys that occurs. If an error occurs when parsing a maths expression this will appear as two errors, one for the error in the maths parser and then one for the key where the error occurs
