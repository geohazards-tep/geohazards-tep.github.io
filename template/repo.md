### [geohazards-tep](https://github.com/geohazards-tep) / [dcs-doris-l1-coseismic](https://github.com/geohazards-tep/dcs-doris-l1-coseismic)

|    Language   | Created       | Last update | Stars          | Forks          | 
|:-------------:|:-------------:|:-----------:|:--------------:|:--------------:|
#|  language  | ` creation`  | ` update`  | ` link.stars` | ` link.forks` |


## Getting Started 

To run this application, you will need a Developer Cloud Sandbox that can be requested from [Terradue's Portal](http://www.terradue.com/partners), provided user registration approval. 

## Installation

#### Using rpms
Log on the developer sandbox. Download the rpm package from https://github.com/geohazards-tep/dcs-doris-l1-coseismic/releases . Install the package by running these commands in a shell:

```bash
sudo yum -y downgrade geos-3.3.2
sudo yum -y install dcs-doris-l1-coseismic-<version>-ciop.x86_64.rpm
```

### Using maven

Log on the developer sandbox and run these commands in a shell:

```bash
sudo yum -y downgrade geos-3.3.2
sudo yum -y install adore-t2

cd
git clone git@github.com:geohazards-tep/dcs-doris-l1-coseismic.git

cd dcs-doris-l1-coseismic
mvn install
```

### Submitting the workflow

Run this command in a shell:

```bash
ciop-simwf
```

Or invoke the Web Processing Service via the Sandbox dashboard providing a master/slave product url and a project name (e.g. LAquila)
