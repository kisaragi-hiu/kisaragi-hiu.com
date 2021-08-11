let cjkFallback = [
  "Noto Sans CJK TC",
  "Microsoft Jhenghei",
  "Microsoft Yahei",
  "Meiryo",
  "Malgun Gothic",
];

module.exports = {
  purge: {
    content: ["./templates/**/*.html"],
    options: { safeList: { deep: [/highlight$/] } },
  },
  mode: "jit",
  darkMode: false, // or 'media' or 'class'
  theme: {
    fontFamily: {
      sans: ["Inter", ...cjkFallback, "sans-serif"],
      // serif: ["Equity"],
      mono: [
        "Overpass Mono",
        "Noto Sans Mono CJK",
        ...cjkFallback,
        "monospace",
      ],
    },
    fontSize: {
      xs: ["0.75rem", { lineHeight: "1rem" }],
      sm: ["12pt", { lineHeight: "1.25rem" }],
      base: ["14pt", { lineHeight: "1.6" }],
      lg: ["16pt", { lineHeight: "1.75rem" }],
      xl: ["24pt", { lineHeight: "1.6" }],
      "2xl": ["36pt", { lineHeight: "2" }],
      "3xl": ["1.875rem", { lineHeight: "2.25rem" }],
      "4xl": ["2.25rem", { lineHeight: "2.5rem" }],
      "5xl": ["3rem", { lineHeight: "1" }],
      "6xl": ["3.75rem", { lineHeight: "1" }],
      "7xl": ["4.5rem", { lineHeight: "1" }],
      "8xl": ["6rem", { lineHeight: "1" }],
      "9xl": ["8rem", { lineHeight: "1" }],
    },
    extend: {
      // https://github.com/tailwindlabs/tailwindcss/discussions/1361
      colors: {
        primary: "#0d0d0d",
        secondary: "#333333",
        special: "#cdecff",
        "special-strong": "#246084",
        "special-light": "#cdecff50",
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
};
