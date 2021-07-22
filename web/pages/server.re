open Tyxml;

let render = (~prefix, ~header=[], ()) => {
  let header = [
    <a href={prefix}> 
      <img id="ocaml-logo" height="32" alt="OCaml" src={prefix ++ "static/logo1.jpeg"} />
      <br/>
      "  docs"
    </a>,
    <a href={prefix ++ "packages/"}> "Packages" </a>,
    ...header,
  ];
  let header_list = List.map(hd => <li> hd </li>, header);
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <link rel="shortcut icon" href={prefix ++ "static/img/favicon.ico"} type_="image/x-icon" />
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" />
      <script defer="defer" src={prefix ++ "static/js/main.js"}></script>
      <link rel="stylesheet" href={prefix ++ "static/main.css"} />
      <link rel="stylesheet" href={prefix ++ "static/css/styles.css"} />
      <title>{"Opam Packages" |> Html.txt}</title>
    </head>
    <body>
      <section id="overlay">
        <div class_="loader loader-1">
          <div class_="loader-outter"></div>
          <div class_="loader-inner"></div>
        </div>
      </section>
      <header id="header"> 
        <nav> 
          <ul> 
            ...header_list 
          </ul> 
        </nav>
      </header>
      <div id="content">
        <main class_="main">
          <section class_="fixed">
            <div class_="fixed-data" id="legends">
              <span>{"A" |> Html.txt}</span>
              <span>{"B" |> Html.txt}</span>
              <span>{"C" |> Html.txt}</span>
              <span>{"D" |> Html.txt}</span>
              <span>{"E" |> Html.txt}</span>
              <span>{"F" |> Html.txt}</span>
              <span>{"G" |> Html.txt}</span>
              <span>{"H" |> Html.txt}</span>
              <span>{"I" |> Html.txt}</span>
              <span>{"J" |> Html.txt}</span>
              <span>{"K" |> Html.txt}</span>
              <span>{"L" |> Html.txt}</span>
              <span>{"M" |> Html.txt}</span>
              <span>{"N" |> Html.txt}</span>
              <span>{"O" |> Html.txt}</span>
              <span>{"P" |> Html.txt}</span>
              <span>{"Q" |> Html.txt}</span>
              <span>{"R" |> Html.txt}</span>
              <span>{"S" |> Html.txt}</span>
              <span>{"T" |> Html.txt}</span>
              <span>{"U" |> Html.txt}</span>
              <span>{"V" |> Html.txt}</span>
              <span>{"W" |> Html.txt}</span>
              <span>{"X" |> Html.txt}</span>
              <span>{"Y" |> Html.txt}</span>
              <span>{"Z" |> Html.txt}</span>
            </div>
          </section>
          <section>
            <div class_="container">
              <h1 class_="title">{"Opam Packages" |> Html.txt}</h1>
              <div class_="filters">
                <input type_="text" name="search" placeholder="Search By Name..." id="filter" />
                <button id="search">{"Search" |> Html.txt}</button>
                <button id="get_deps">{"Get Dependants" |> Html.txt}</button>
              </div>
              <table  id="clear_tbody">
                <thead>
                  <tr>
                    <th>{"Name" |> Html.txt}</th>
                    <th>{"Description" |> Html.txt}</th>
                    <th>{"Version" |> Html.txt}</th>
                  </tr>
                </thead>
                <tbody id="opam_packages"></tbody>
              </table>
            </div>
          </section>

          <div class_="pagination">
            <a href="#">{"<< " |> Html.txt}</a>
            <a href="#">{"1" |> Html.txt}</a>
            <a href="#">{"2" |> Html.txt}</a>
            <a href="#">{"3" |> Html.txt}</a>
            <a href="#">{"4" |> Html.txt}</a>
            <a href="#">{"5" |> Html.txt}</a>
            <a href="#">{"6" |> Html.txt}</a>
            <a href="#">{" >>" |> Html.txt}</a>
          </div>
          
        </main>
        <footer class_="footer">
          <div class_="container">
            <div class_="footer-wrapper">
              <p>{"Ocaml © 2021" |> Html.txt}</p>
            </div>
          </div>
        </footer>
      </div>
      <noscript>{"Sorry, you need to enable JavaScript to see this page." |> Html.txt}</noscript>
    </body>
  </html>;
}

let v(~prefix) = Lwt.return(Fmt.to_to_string(Html.pp(), render(~prefix, ())));
