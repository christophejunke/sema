#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef struct context_s {
	sem_t checkpoint;
	sem_t wake_up;
} context_t;

void perr (char* label, int value)
{
	printf("%s: %d\n", label, value);
	if (value) {
		perror("Aborting");
		exit(-1);
	}
}

void info(char* text)
{
	printf("%s\n", text);
}

void pstart(char* label)
{
	printf("Start %s\n", label);
}

void pend(char* label)
{
	printf("End %s\n", label);
}

void* controller_handler (void* data)
{
	context_t* context = data;
	pstart("controller");
	info("waiting all");
	sem_wait(&(context->checkpoint));
	sem_wait(&(context->checkpoint));
	info("waking up");
	sem_post(&(context->wake_up));
	sem_post(&(context->wake_up));
	pend("controller");
	return NULL;
}

void* task1_handler (void* data)
{
	context_t* context = data;
	pstart("task1_handler");
	usleep(200);
	info("[t1] ready");
	sem_post(&(context->checkpoint));
	sem_wait(&(context->wake_up));
	info("[t1] race!");
	pend("task1_handler");
	return NULL;
}

void* task2_handler (void* data)
{
	context_t* context = data;
	pstart("task2_handler");
	usleep(200);
	info("[t2] ready");
	sem_post(&(context->checkpoint));
	sem_wait(&(context->wake_up));
	info("[t2] race!");
	pend("task2_handler");
	return NULL;
}

int main(void)
{
	pstart("main");
	context_t context;

	perr("checkpoint_init", sem_init(&(context.checkpoint), 0, 0));
	perr("wake_up_init", sem_init(&(context.wake_up), 0, 0));
	pthread_t controller;
	perr("pthread_create controller",
	     pthread_create(&controller, NULL, controller_handler, &context));

	pthread_t task1;
	perr("pthread_create task1",
	     pthread_create(&task1, NULL, task1_handler, &context));

	pthread_t task2;
	perr("pthread_create task2",
	     pthread_create(&task2, NULL, task2_handler, &context));

	pthread_join(controller, NULL);
	pthread_join(task1, NULL);
	pthread_join(task2, NULL);

	perr("destroy_checkpoint", sem_destroy(&(context.checkpoint)));
	perr("destroy_wake_up", sem_destroy(&(context.wake_up)));

	pend("main");
	return 0;
}
