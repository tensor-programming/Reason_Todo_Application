type todo = {
  id: int,
  value: string,
  completed: bool
};

type state = {todos: list(todo)};

type action =
  | AddTodo(string)
  | CheckTodo(int)
  | DeleteTodo(int);

let toString = ReasonReact.stringToElement;

let lastId = ref(0);

let newTodo = todo => {
  lastId := lastId^ + 1;
  {id: lastId^, completed: false, value: todo};
};

let checkTodo = (id, todos) =>
  List.map(
    todo => todo.id === id ? {...todo, completed: ! todo.completed} : todo,
    todos
  );

let deleteTodo = (id, todos) => List.filter(todo => todo.id !== id, todos);
let valueFromEvent = e : string => (
                                     e
                                     |> ReactEventRe.Form.target
                                     |> ReactDOMRe.domElementToObj
                                   )##value;
module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _) => {
    ...component,
    initialState: () => "",
    reducer: (newTodo, _) => ReasonReact.Update(newTodo),
    render: ({state: todo, reduce}) =>
      <input
        className="Input"
        value=todo
        _type="text"
        placeholder="What is it that you want todo?"
        onChange=(reduce(e => valueFromEvent(e)))
        onKeyDown=(
          (e) =>
            if (ReactEventRe.Keyboard.key(e) == "Enter") {
              onSubmit(todo);
              (reduce(() => ""))();
            }
        )
      />
  };
};

module TodoItem = {

  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~todo: todo, ~onToggle, ~clickDelete, _) => {
    ...component,
    render: _self => 
      <div className="item" onClick=(_e => onToggle())> 
      <input 
        className="checkbox"
        _type="checkbox"
        checked=(Js.Boolean.to_js_boolean(todo.completed))
      />
      <label> (toString(todo.value))</label>
      <input
        _type="button"
        className="button"
        value="x"
        onClick=(_e => clickDelete())
      />
    </div>
  };
};



let component = ReasonReact.reducerComponent("App");
let make = _children => {
  ...component,
  initialState: () => {
    todos: []
  },
  reducer: (action, {todos}) =>
    switch action {
    | AddTodo(value) => ReasonReact.Update({todos: [newTodo(value), ...todos]})
    | CheckTodo(id) => ReasonReact.Update({todos: checkTodo(id, todos)})
    | DeleteTodo(id) => ReasonReact.Update({todos: deleteTodo(id, todos)})
    },
  render: ({state: {todos}, reduce}) => {
    let length = List.length(todos);
    let counter = 
      length > 1 ?
        string_of_int(length) ++ " todos" : string_of_int(length) ++ " todo";

    <div className="App">
      <h3> (toString("Todo App")) </h3>
      <Input onSubmit=(reduce(todo => AddTodo(todo))) />
      <div className="todoList">
        (
          List.map(
            todo => 
              <TodoItem
                key=(string_of_int(todo.id))
                todo
                onToggle=(reduce(() => CheckTodo(todo.id)))
                clickDelete=(reduce(() => DeleteTodo(todo.id)))
              />,
            todos
          )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </div>
      <div className="footer" > (toString(counter)) </div>
    </div>
  }
};