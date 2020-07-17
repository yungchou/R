# https://github.com/Azure/AzureVM

p<-c('AzureRMR','AzureGraph','AzureStore','AzureVM')
if(!require(p)){install.packages(p, dependency=TRUE, clean=TRUE); library(p, character.only=TRUE)}

# set your Azure organization and subscription details here
tenant <- "mytenant"
sub_id <- "12345678-aaaa-bbbb-cccc-0123456789ab"


az <- create_azure_login()

sub <- AzureRMR::get_azure_login()$get_subscription("sub_id")

# calling create_vm() from a subscription object will create the VM in its own resource group
# default is an Ubuntu 18.04 VM, size Standard_DS3_v2, login via SSH key
# call sub$list_vm_sizes() to get the sizes available in your region
vm <- sub$create_vm("myubuntuvm", user_config("myname", "~/.ssh/id_rsa.pub"),
                    location="australiaeast")

# some resources used by the VM
vm$get_vnet()
vm$get_public_ip_resource()
vm$get_disk("os")

# run a shell script or command remotely (will be PowerShell on a Windows VM)
vm$run_script("echo hello world! > /tmp/hello.txt")

# ... and stop it
vm$stop()

# ... and resize it
vm$resize("Standard_DS4_v2")

# ... and delete it (this can be done asynchronously for a VM in its own group)
vm$delete()


sub$create_vm("mydsvm", user_config("myname", "~/.ssh/id_rsa.pub"), config="ubuntu_dsvm",
              location="australiaeast")

sub$create_vm("mywinvm", user_config("myname", password="Use-strong-passwords!"), config="windows_2019",
              location="australiaeast")

# Windows Server 2016, with a 500GB datadisk attached, not publicly accessible
sub$create_vm("mywinvm2", user_config("myname", password="Use-strong-passwords!"),
              size="Standard_DS4_v2", config="windows_2016", datadisks=500, ip=NULL,
              location="australiaeast")

# Ubuntu DSVM, GPU-enabled
sub$create_vm("mydsvm", user_config("myname", "~/.ssh/id_rsa.pub"), size="Standard_NC12s_v2",
              config="ubuntu_dsvm",
              location="australiaeast")

# Red Hat VM, serving HTTP/HTTPS
sub$create_vm("myrhvm", user_config("myname", "~/.ssh/id_rsa.pub"), config="rhel_8",
              nsg=nsg_config(list(nsg_rule_allow_http, nsg_rule_allow_https)),
              location="australiaeast")


## custom VM configuration: Windows 10 Pro 1903 with data disks
## this assumes you have a valid Win10 desktop license
user <- user_config("myname", password="Use-strong-passwords!")
image <- image_config(
  publisher="MicrosoftWindowsDesktop",
  offer="Windows-10",
  sku="19h1-pro"
)
datadisks <- list(
  datadisk_config(250, type="Premium_LRS"),
  datadisk_config(1000, type="Standard_LRS")
)
nsg <- nsg_config(
  list(nsg_rule_allow_rdp)
)
sub$create_vm("mywin10vm", user,
              config=vm_config(
                image=image,
                keylogin=FALSE,
                datadisks=datadisks,
                nsg=nsg,
                properties=list(licenseType="Windows_Client")
              ),
              location="australiaeast"
)

# default is Ubuntu 18.04 scaleset, size Standard_DS1_v2
sub$create_vm_scaleset("myubuntuss", user_config("myname", "~/.ssh/id_rsa.pub"), instances=5,
                       location="australiaeast")

# Windows Server 2019
sub$create_vm_scaleset("mywinss", user_config("myname", password="Use-strong-passwords!"), instances=5,
                       config="windows_2019_ss",
                       location="australiaeast")

# RHEL scaleset, serving HTTP/HTTPS
sub$create_vm_scaleset("myrhelss", user_config("myname", "~/.ssh/id_rsa.pub"), instances=5,
                       config="rhel_8_ss",
                       nsg=nsg_config(list(nsg_rule_allow_http, nsg_rule_allow_https)),
                       location="australiaeast")

# Ubuntu DSVM, GPU-enabled, public instances, no load balancer or autoscaler
sub$create_vm_scaleset("mydsvmss", user_config("myname", "~/.ssh/id_rsa.pub"), instances=5,
                       size="Standard_NC6", config="ubuntu_dsvm_ss",
                       options=scaleset_options(public=TRUE),
                       load_balancer=NULL, autoscaler=NULL,
                       location="australiaeast")

# Large Debian scaleset (multiple placement groups), using spot VMs (low-priority)
# need to set the instance size to something that supports low-pri
sub$create_vm_scaleset("mylargess", user_config("myname", "~/.ssh/id_rsa.pub"), instances=10,
                       size="Standard_DS3_v2", config="debian_9_backports_ss",
                       options=scaleset_options(priority="spot", large_scaleset=TRUE),
                       location="australiaeast")

# this will create a pool of up to 10 processes that talk to the scaleset
mylargess$run_script("echo hello world! > /tmp/hello.txt")

## VM and scaleset in the same resource group and virtual network
# first, create the resgroup
rg <- sub$create_resource_group("rgname", "australiaeast")

# create the master
rg$create_vm("mastervm", user_config("myname", "~/.ssh/id_rsa.pub"))

# get the vnet resource
vnet <- rg$get_resource(type="Microsoft.Network/virtualNetworks", name="mastervm-vnet")

# create the scaleset
# since the NSG is associated with the vnet, we don't need to create a new NSG either
rg$create_vm_scaleset("slavess", user_config("myname", "~/.ssh/id_rsa.pub"),
                      instances=5, vnet=vnet, nsg=NULL)

