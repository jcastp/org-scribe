# Writing Project Templates

This directory contains templates for creating writing projects.

## Structure

```
writing-templates/
└── novel/                          # Novel project template
    ├── README.org.template         # Project overview
    ├── novela.org.template         # Main manuscript
    ├── revision.org.template       # Revision tracking
    ├── estadisticas.org.template   # Writing statistics
    ├── .gitignore                  # Git ignore file (copied as-is)
    ├── plan/
    │   ├── personajes.org.template
    │   ├── localizaciones.org.template
    │   ├── objetos.org.template
    │   ├── trama.org.template
    │   └── cronologia.org.template
    └── notas/
        ├── notas.org.template
        └── investigacion.org.template
```

## Template Variables

Templates use variable substitution with the following placeholders:

- `${TITLE}` - The novel/project title
- `${AUTHOR}` - The author's name (from `user-full-name`)
- `${DATE}` - Current date in YYYY-MM-DD format

### Example

```org
#+TITLE: ${TITLE}
#+AUTHOR: ${AUTHOR}
#+DATE: ${DATE}

* Project started on ${DATE}
```

When creating a project with title "My Novel", this becomes:

```org
#+TITLE: My Novel
#+AUTHOR: Javier Castilla
#+DATE: 2025-11-08

* Project started on 2025-11-08
```

## Customizing Templates

### Editing Existing Templates

1. Open any `.template` file in Emacs
2. Edit the content as you would a normal org file
3. Use `${TITLE}`, `${AUTHOR}`, `${DATE}` where you want substitution
4. Save the file

You can also use:
```
M-x my/edit-novel-templates
```

This opens the template directory in dired for easy navigation.

### Files Without Substitution

Files without the `.template` extension (like `.gitignore`) are copied as-is without variable substitution.

## Creating New Template Sets

To create a new template type (e.g., short story, screenplay):

1. Create a new directory: `writing-templates/short-story/`
2. Add template files with `.template` extension
3. Modify `writing_project.el` to support multiple template types (optional)

## Template File Naming

- Files ending in `.template` will have the extension removed in the final project
- Example: `README.org.template` → `README.org`
- Files without `.template` are copied with their original name

## Usage

From Emacs:

```
M-x my/create-novel-project
```

Then enter:
1. Base directory (where to create the project)
2. Novel title

The system will:
1. Create a directory with the novel title
2. Copy all template files
3. Replace all `${VARIABLE}` placeholders
4. Open the README.org file

## Benefits

- **Easy to customize** - Edit templates as regular org files
- **No Emacs Lisp needed** - Anyone can modify templates
- **Version control friendly** - Templates are plain text
- **Modular** - Each file is independent
- **Flexible** - Add new templates by creating new files
