#ifndef __PIPEBIDIRECCIONAL_H__
#define __PIPEBIDIRECCIONAL_H__


struct _pipe;
 
typedef struct _pipe pipe_b;

pipe_b* pipe_b_create();

void pipe_b_init(pipe_b* pipe);

void pipe_b_child_write(pipe_b* pipe, char* str);

void pipe_b_parent_write(pipe_b* pipe, char* str);

void pipe_b_parent_read(pipe_b* pipe);

void pipe_b_child_read(pipe_b* pipe);

void pipe_b_parent_close_end(pipe_b* pipe);

void pipe_b_child_close_end(pipe_b* pipe);

void pipe_b_close_begin_child(pipe_b* pipe);

void pipe_b_close_begin_parent(pipe_b* pipe);

void pipe_b_destroy(pipe_b* pipe);
#endif