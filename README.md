# cmdstan-wrapper

My current setup for using CmdStan in R

# Getting started

This code references cmdstan binaries which are contained in git submodules.

Before running the code, you will want to initialize and/or update submodules.

After cloning the repository and/or after pulling changes from a remote repo:

```
git submodule update --init --recursive
```

# Requirements

There are two sets of requirements for the code in this repository to work.

1. A working version of `cmdstan`. Note that the code will expect this to be located where the submodule lives
    - Try to build the binaries (if you're on linux/mac, it may work as-is): `cd cmdstan && make build`
    - See the [cmdstan installation instructions](http://mc-stan.org/users/interfaces/cmdstan) for system prerequisites (`build-essentials`, among other things)
      (for one of my ubuntu boxes, I had to customize the makefile)
    - We can allow for a customized location if desired; right now it wasn't a high priority.
      - To edit this manually, see [stan_helpers.function.R](stan_helpers.function.R)

2. A bunch of R packages, listed in [setup.R](setup.R)
    - You can install these using (from the working directory): `Rscript setup.R`
    - Note that this will install both Rstan and rstanarm. Not all of the packages are required; this was copied over from a working project.
    - The [.Rprofile](.Rprofile) file in the root of this directory will set up a `Rlib` folder where new packages will be installed by default.
    - (note, also, if you're using `conda`, it will skip the `Rlib` and install install to the conda env)

# Workflow

Fitting a stan model proceeds in a few steps (similar to what one would do when using `rstan`).

1. Prepare stan_data to be fit; save as a named list or environment.
    - I keep "other information" about my data preparation (ie factor labels, spline bases, etc) in attributes of the data.
    - If you do this, this information will be archived along with your input data 
2. Call `fit_stan_model()` function, within [stan_helpers.functions.R](stan_helpers.functions.R).
3. Evaluate the model fit. Personally I have an Rmd template file I use to produce a static MD for each model fit. That process isn't included here.

# A note about this CmdStan wrapper vs using Rstan

The scripts in this repo largely replicate the functionality of Rstan -- I have found, however, several benefits to this approach:

1. I can checkpoint the version of CmdStan I want to reference, by pointing the git submodule included here to a particular commit. 
2. I have somewhat better job control due to the simplicity of the code; I find the executing chains are more stable in this version & killing a job correctly kills all child processes
3. This code will maintain an archive of previous stan fits. The stan input data, chains, summary, and model fits are stored together for more reproducible execution (there are better ways to do this; this is a version that works for me).
4. I can easily call CmdStan directly or share the files with another researcher who can use their preferred stan interface.
5. I personally like that the same call to `fit_stan_model` will not re-execute runs that have already completed. I am a big fan of caching.

The code to execute via CmdStan is in the [stan_helpers.functions.R](stan_helpers.functions.R) file.

