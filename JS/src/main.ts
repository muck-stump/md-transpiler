import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const saver = document.getElementById("save-button")!;
const title = document.getElementById("title")!;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, save: false};
};


const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, markdown: value})),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value})),
);

const save$: Observable<Action> = fromEvent(saver, "click").pipe(
    map( _ => ( (s:State) => ({...s, save: true}))
))

const title$: Observable<Action> = fromEvent(title, "input").pipe(
    map((event) => (event.target as HTMLInputElement).value ),
    map( title => ( (s:State) => ({...s, title: title }) ))
)

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.save + "\n" + s.title + "\n" + s.markdown
        // `{"save":${s.save},"markdown" : "${s.markdown}","title":"${s.title}"}`
        // JSON.stringify({save: s.save, markdown: `\"${s.markdown}\"`, title: s.title})
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

// `{"save":${s.save},"markdown" : "${s.markdown}","title":"a"}`

// i give more info to body (not just s.markwon) s.markdown so it knows if i hit save

const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    title: "",
};

const refreshers$ = merge(input$, checkboxStream$, title$).pipe(
    map((reducer: Action) => compose(reducer)(resetState))
)

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(refreshers$, save$)
        .pipe(
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + value.HTML;
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = value.HTML;
                }
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
