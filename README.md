# todosrv

REST APIs server application for managing todo list with a simple auth system.

## Run

To run the application just type:

```bash
$ docker compose up --build
```

in your shell, and you can access endpoints of the application using
[http://localhost:9090/todos](http://localhost:9090/todos) url.

## APIs

### Authorization

To authorize your requests you should use one of the following **tokens**:

- `g2gCbQAAACRjMTAwZDgyOS1lNDI3LTRiZDctYjJjZS0zNWJhZGNmMTJkY2RrAAZzdHVwaWQ=` (user *root*)
- `g2gCbQAAACQzZDU0OGIwMS1jOWYzLTQ4NDMtYTJjOC00YTdlODEzOWQ4NWJrAAttb3JlX3N0dXBpZA==` (user *f1sty*)

Pass one of this tokens in your `authorization` header, e.g.:

```bash
$ curl -c cookiejar -L -H 'Authorization: Bearer g2gCbQAAACRjMTAwZDgyOS1lNDI3LTRiZDctYjJjZS0zNWJhZGNmMTJkY2RrAAZzdHVwaWQ=' http://localhost:9090/todos | jq '.'
```

A token is **required**. If you change one valid token to another, you will automatically login
under a new identity. I didn't want to overcomplicate the authorization flow, so there is no
separate endpoint for login and logout. I've also had to fork and modify
[cowboy_session](https://github.com/f1sty/cowboy_session) to handle my case.

### Endpoints

- **GET** `/todos` list all the todos for the current user:

Response:

```json
{
  "todos": [
    {
      "text": "wash the car",
      "id": "a1383dab-ca55-4a07-abca-f4b0a3399c9c",
      "done": false
    },
    {
      "text": "ban annoying redditors",
      "id": "26afd816-89d6-4397-90ee-6926747122e2",
      "done": false
    },
    {
      "text": "eat some ramen",
      "id": "0890b745-f90a-4c35-9e60-b5838f7280f2",
      "done": false
    }
  ]
}
```

- **GET** `/todos/[:todo_id]` get a particular for the current user:

Response:

```json
{
  "text": "wash the car",
  "id": "a1383dab-ca55-4a07-abca-f4b0a3399c9c",
  "done": false
}
```

- **POST** `/todos` create a new todo for the current user:

Body:

```json
{
  "text": "cut the grass"
}
```

Response: Empty body with **201 Created** HTTP response status code.

- **PATCH** `/todos/[:todo_id]` update a particular todo for the current user (you can only update `done` field):

Body:

```json
{
  "done": true
}
```

Response: Empty body with **204 No Content** HTTP response status code.

- **DELETE** `/todos/[:todo_id]` delete a particular todo for the current user:

Response: Empty body with **204 No Content** HTTP response status code.
