# A local proxy for Forex rates

Build a local proxy for getting Currency Exchange Rates

## Requirements

[Forex](https://github.com/paidy/interview/tree/master/forex-mtl) is a simple application that acts as a local proxy for
getting exchange rates. It's a service that can be consumed by other internal services to get the exchange rate between
a set of currencies, so they don't have to care about the specifics of third-party providers.

We provide you with an initial scaffold for the application with some dummy interpretations/implementations. For
starters we would like you to try and understand the structure of the application, so you can use this as the base to
address the following use case:

* The service returns an exchange rate when provided with 2 supported currencies
* The rate should not be older than 5 minutes
* The service should support at least 10,000 successful requests per day with 1 API token

Please note the following drawback of the [One-Frame service](https://hub.docker.com/r/paidyinc/one-frame):

> The One-Frame service supports a maximum of 1000 requests per day for any given authentication token.
>
>

## Getting started

Steps to run the application for review and tests

we can test it in two possible ways dockeriz forex service and deploy it along with `oneframe` using a docker and run
the tests `or ` run this service on local.

I personally prefer the 2nd method as I am assuming the one-frame is already running in some environment and we can just
plug forex remotely to test it out and run some scenarios.

1. run the docker container for one-frame( if not already running in some environment  ) and make sure the uri for the
   running container is added in application.conf update if needed

```bash
docker pull paidyinc/one-frame
```

```bash
docker run -p 8080:8080 paidyinc/one-frame
```

I am assuming the same token shared along with task would be the one that will work for the container running on your
machine or in prod if not please do update that along with Uri as well

2. once all the changes required in application.conf run the forex service

```bash
sbt run
```

once it up and running you can make api call for get as much as you need to test the service as per requirements

## Solution Approach

### Phase 1: In-Memory Cache Implementation

I started with a simple in-memory cache that worked well for single instances but had obvious limitations for distributed deployment.

### Phase 2: Shared Cache(Redis Implementation with Distributed Locking)
I realized that deploying multiple nodes would create problems:
- Each node would have its own cache
- Multiple nodes might call OneFrame API simultaneously
- No coordination between instances
- Quota exhaustion from redundant calls
- Implemented Redis as shared cache with distributed locking to solve concurrency issues. Only one node can acquire the lock and refresh rates while others wait and use the fresh data from Redis.
- Configured the system to check Redis first on startup, reducing unnecessary OneFrame API calls and improving reliability.

### Phase 3: Fault Tolerance

I built comprehensive fault tolerance so the service never crashes:
- Try Redis cache first
- if redis not available don't make any external calls to third party and save quota
- Auto-reconnect when services come back online
- Fetch Rates when redis is connected

### Phase 4: Health Monitoring

Added health check endpoints to provide visibility into service state, quota consumption, and whether the service can currently serve rates to customers.

### Phase 5: Testing

Due to other obligations I was unable to spend much time on writing tests for excellent coverage at least 80% as i should have, I mainly focused on writing test for major flows. 204 tests with 100% pass rate and 19.89% statement coverage.

## Key Features

- Cluster-safe design with distributed coordination
- Fault tolerance with graceful degradation
- Auto-recovery when services return
- Health monitoring and admin controls for quota refresh along with auto rest on each day at 00:00:00 UTC
- Comprehensive test coverage of critical flows

### Quick Start
1. Pull redis container:
```bash
docker pull redis:latest
```
2. Start Redis server
```bash
docker run --name redis-server -p 6379:6379 -d redis:latest
```
3. either pull one-frame and run it on 8080 or add the configs for the live server
4. Application  will start on http://localhost:8081 if we start it as is else host port and timeout can be configured in `src/main/resources/application.conf`
5. Run `sbt compile` then `sbt run`


### API Endpoints
- Rate lookup: `/rates?from=USD&to=EUR` and `/rates`
- Health check: `/forex/health`
- Readiness probe: `/forex/ready`
- Admin quota refresh: `POST /forex/admin/quota/refresh`

## Testing

Run tests with `sbt test`

**Results**: 204 tests, 100% pass rate