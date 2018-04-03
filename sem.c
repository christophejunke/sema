#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

sem_t checkpoint;
sem_t tick;

pthread_t controller;
pthread_t task1;
pthread_t task2;

void perr (char* label, int value)
{
        printf("%s: %d\n", label, value);
        if (value) {
                perror("Aborting");
                exit(-1);
        }
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
        pstart("controller");
        printf("waiting all\n");
        sem_wait(&checkpoint);
        sem_wait(&checkpoint);
        printf("waking up\n");
        sem_post(&tick);
        sem_post(&tick);
        pend("controller");
        return NULL;
}

void* task1_handler (void* data)
{
        pstart("task1_handler");
        usleep(200);
        printf("[t1] ready\n");
        sem_post(&checkpoint);
        sem_wait(&tick);
        printf("[t1] race!\n");
        pend("task1_handler");
        return NULL;
}

void* task2_handler (void* data)
{
        pstart("task2_handler");
        usleep(200);
        printf("[t2] ready\n");
        sem_post(&checkpoint);
        sem_wait(&tick);
        printf("[t2] race!\n");        
        pend("task2_handler");
        return NULL;
}

int main(void)
{
        printf("Main start\n");
        perr("checkpoint_init", sem_init(&checkpoint, 0, 0));
        perr("tick_init", sem_init(&tick, 0, 0));

        perr("pthread_create controller", pthread_create(&controller, NULL, controller_handler, NULL));
        perr("pthread_create task1", pthread_create(&task1, NULL, task1_handler, NULL));
        perr("pthread_create task2", pthread_create(&task2, NULL, task2_handler, NULL));

        pthread_join(controller, NULL);
        pthread_join(task1, NULL);
        pthread_join(task2, NULL);
        
        perr("destroy_checkpoint", sem_destroy(&checkpoint));
        perr("destroy_tick", sem_destroy(&tick));
        printf("Main stop\n");
        return 0;
}
