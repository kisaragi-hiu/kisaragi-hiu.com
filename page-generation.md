# Category Page Generation

Categories are first defined in `index.ptree`, under the `◊category` tree.

```
◊category{
    category/opinions.html
    category/tutorials.html
}
```

Then code in `before-pollen.rkt` looks at that, and writes each category's Pollen source to `category`.

Pollen rendering is only called after page generation is complete. This is defined in the `build` function in `build.sh`.
