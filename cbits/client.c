#include "client.h"

#include <arpa/inet.h>
#include <stdint.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>

#define PROTOCOL_HELLO "PROTOCOL10: HELLO, I AM: "

static __thread struct {
    pid_t pid;                  /* TODO: Document that this identifies fork()ed threads */
    int connection_fd;
    enum need need;
    uint32_t msg_counter;
    bool awaiting_go;
} thread_state = {-1, -1, -1, 0, false};

static int gettid(void)
{
    #ifdef __APPLE__
        return pthread_mach_thread_np(pthread_self());
    #else
        return syscall(__NR_gettid);
    #endif
}

static bool send_all(int fd, const char *buf, size_t size)
{
    ssize_t send_rc = send(fd, buf, size, 0);
    return (size_t)send_rc == size;
}

static bool send_size(int fd, size_t size)
{
    uint32_t size32 = ntohl(size);
    return send_all(fd, PS(size32));
}

static int connect_master(const char *need_str)
{
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    ASSERT(-1 != fd);

    char *env_sockaddr = getenv(ENVVARS_PREFIX "MASTER_UNIX_SOCKADDR");
    ASSERT(env_sockaddr);

    char *env_job_id = getenv(ENVVARS_PREFIX "JOB_ID");
    ASSERT(env_job_id);

    struct sockaddr_un addr = {
        .sun_family = AF_UNIX,
    };
    ASSERT(strlen(env_sockaddr) < sizeof addr.sun_path);
    strcpy(addr.sun_path, env_sockaddr);

    int connect_rc = connect(fd, (struct sockaddr*) &addr, sizeof addr);
    ASSERT(0 == connect_rc);

    char hello[strlen(PROTOCOL_HELLO) + strlen(env_job_id) + 24]; /* TODO: Avoid magic constant */
    hello[sizeof hello-1] = 0;

    int len = snprintf(hello, sizeof hello-1, PROTOCOL_HELLO "%d:%d:%s:%s", getpid(), gettid(),
                       env_job_id, need_str);
    ASSERT(send_size(fd, len));

    bool hello_success = send_all(fd, hello, len);
    ASSERT(hello_success);

    return fd;
}

int client_make_connection(enum need need)
{
    pid_t pid = getpid();
    if(pid != thread_state.pid) {
        int fd = connect_master(need == HOOK ? "HOOK" : "HINT");
        if(-1 == fd) return -1;
        thread_state.connection_fd = fd;
        thread_state.pid = pid;
        thread_state.need = need;
        thread_state.awaiting_go = true;
        thread_state.msg_counter = 0;
        if(!await_go()) return -1;
    }
    ASSERT(thread_state.need == need);
    return thread_state.connection_fd;
}

static int connection(void)
{
    return client_make_connection(HOOK);
}

static int assert_connection(void)
{
    ASSERT(getpid() == thread_state.pid);
    return thread_state.connection_fd;
}

static void recv_n(void *buf, uint32_t n)
{
    uint32_t left = n;
    while (left > 0) {
        ssize_t rc = recv(assert_connection(), buf, left, 0);
        if ((rc == -1) && (errno == EINTR)) continue;
        ASSERT(rc >= 0);
        left -= (uint32_t)rc;
    }
}

static bool await_go_internal(void)
{
    ASSERT(thread_state.awaiting_go);
    {
        char buf[2];
        recv_n(PS(buf));
        ASSERT(0 == memcmp("GO", PS(buf)));
    }
    {
        uint32_t recv_counter;
        recv_n(PS(recv_counter));
        ASSERT(recv_counter == thread_state.msg_counter);
    }
    thread_state.awaiting_go = false;
    return true;
}

bool await_go(void)
{
    return await_go_internal();
}

bool client__send_hooked(bool is_delayed, const char *buf, size_t size)
{
    ASSERT(!thread_state.awaiting_go);
    int fd = connection();
    ASSERT(0 <= fd);

    thread_state.msg_counter++;
    ASSERT(send_size(fd, sizeof(is_delayed)+sizeof(thread_state.msg_counter)+size));
    ASSERT(send_all(fd, PS(is_delayed)));
    ASSERT(send_all(fd, PS(thread_state.msg_counter)));
    ASSERT(send_all(fd, buf, size));
    thread_state.awaiting_go = true;
    return true;
}
