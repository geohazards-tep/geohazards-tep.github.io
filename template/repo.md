### [geohazards-tep](https://github.com/geohazards-tep) / [dcs-doris-l1-coseismic](https://github.com/geohazards-tep/dcs-doris-l1-coseismic)


|    Language   | Created       | Last update | Stars          | Forks          | 
|:-------------:|:-------------:|:-----------:|:--------------:|:--------------:|
|   | 2014-11-07T08:39:32Z  | 2014-11-07T08:39:54Z  | [0](https://github.com/geohazards-tep/dcs-doris-l1-coseismic/stargazers) | [0](https://github.com/geohazards-tep/dcs-doris-l1-coseismic/network) |


### Getting Started 

To run this application, you will need a Developer Cloud Sandbox that can be requested from [Terradue's Portal](http://www.terradue.com/partners), provided user registration approval. 

### Installation

#### using rpms
Log on the developer sandbox. Download the rpm package from https://github.com/Terradue/dcs-doris-l1-coseismic/releases . Install the package by running these commands in a shell:

```bash
sudo yum -y downgrade geos-3.3.2
sudo yum -y install dcs-doris-l1-coseismic-<version>-ciop.x86_64.rpm
```

#### using maven

Log on the developer sandbox and run these commands in a shell:

```bash
sudo yum -y downgrade geos-3.3.2
sudo yum -y install adore-t2

cd
git clone git@github.com:Terradue/dcs-doris-l1-coseismic.git

cd dcs-doris-l1-coseismic
mvn install
```

### Submitting the workflow

Run this command in a shell:

```bash
ciop-simwf
```

Or invoke the Web Processing Service via the Sandbox dashboard providing a master/slave product url and a project name (e.g. LAquila)

