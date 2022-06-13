let cjkFallback = [
  "Noto Sans CJK TC",
  "Microsoft Jhenghei",
  "Microsoft Yahei",
  "Meiryo",
  "Malgun Gothic",
];

module.exports = {
  darkMode: "class",
  content: ["./templates/**/*.html", "./content/**/*.org"],
  theme: {
    fontFamily: {
      sans: ["Fira Sans", ...cjkFallback, "sans-serif"],
      serif: [
        "'Equity A'",
        "'Noto Serif CJK JP'",
        ...cjkFallback,
        "sans-serif",
      ],
      // serif: ["Equity"],
      mono: [
        "Inconsolata",
        "Noto Sans Mono CJK TC",
        ...cjkFallback,
        "monospace",
      ],
    },
    extend: {
      typography: {
        DEFAULT: {
          // cancelling this out in CSS is even uglier, so just do it here
          css: {
            "blockquote p:first-of-type::before": false,
            "blockquote p:last-of-type::after": false,
          },
        },
      },
      colors: {
        primary: "#0d0d0d",
        secondary: "#333333",
        accent: "#cdadff",
        "accent-strong": "#7E49B6",
        "accent-light": "#cdadff30",
        special: "#cdecff",
        "special-strong": "#246084",
        "special-light": "#cdecff50",
      },
      fill: (theme) => ({
        accent: theme("colors.accent"),
        "accent-strong": theme("colors.accent-strong"),
        special: theme("colors.special"),
        "special-strong": theme("colors.special-strong"),
      }),
      zIndex: { "-5": "-5" },
      // https://github.com/tailwindlabs/tailwindcss/discussions/1361
      boxShadow: {
        DEFAULT: "0 0 0.25rem #00000040",
        md: "0 0 0.25rem #00000070",
        white: "0 0 0.5rem #ffffff",
      },
    },
  },
  plugins: [require("@tailwindcss/typography")],
};
